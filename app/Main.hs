module Main where

-- import qualified Data.Text as T
-- I will port to Data.Text after I get this program working

import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import GHC.Base (maxInt)
import Text.Read (readMaybe)

main :: IO ()
main = interact (show . interpret)

interpret :: String -> Maybe String
interpret = output . execProgram . uncurry initProcessor . parseProgram

data Status = StatusUndefined | StatusLess | StatusEqual | StatusGreater deriving (Eq)

type Registers = M.Map String Int

type Label = String

type Labels = M.Map Label Int

data Var where
  Lit :: Int -> Var
  Reg :: String -> Var
  deriving (Show, Eq)

data Cmd where
  Mov :: Var -> Var -> Cmd
  Inc :: Var -> Cmd
  Dec :: Var -> Cmd
  Add :: Var -> Var -> Cmd
  Sub :: Var -> Var -> Cmd
  Mul :: Var -> Var -> Cmd
  Div :: Var -> Var -> Cmd
  Cmp :: Var -> Var -> Cmd
  Jmp :: Label -> Cmd
  Jne :: Label -> Cmd
  Je :: Label -> Cmd
  Jge :: Label -> Cmd
  Jg :: Label -> Cmd
  Jle :: Label -> Cmd
  Jl :: Label -> Cmd
  Call :: Label -> Cmd
  Ret :: Cmd
  Msg :: [String] -> Cmd
  End :: Cmd
  Illegal :: String -> Cmd
  deriving (Show, Eq)

data Processor = Processor
  { programCounter :: !Int,
    status :: !Status,
    registers :: !Registers,
    stack :: ![Int],
    labels :: !(M.Map Label Int),
    commands :: !(M.Map Int Cmd),
    output :: !(Maybe String),
    ended :: !Bool
  }

initProcessor :: [Cmd] -> Labels -> Processor
initProcessor commands labels =
  Processor
    { programCounter = 0,
      status = StatusUndefined,
      registers = M.empty,
      stack = [],
      labels = labels,
      commands = M.fromList $ zip [0 ..] commands,
      output = Nothing,
      ended = False
    }

execProgram :: Processor -> Processor
execProgram p =
  let command = M.lookup (programCounter p) (commands p)
   in case command of
        Nothing -> p
        Just cmd -> case cmd of
          Mov a b -> cmdMov a b p
          Inc a -> cmdInc a p
          Dec a -> cmdDec a p
          Add a b -> cmdAdd a b p
          Sub a b -> cmdSub a b p
          Mul a b -> cmdMul a b p
          Div a b -> cmdDiv a b p
          Cmp a b -> cmdCmp a b p
          Jmp l -> cmdJmp l p
          Jne l -> cmdJne l p
          Je l -> cmdJe l p
          Jge l -> cmdJge l p
          Jg l -> cmdJg l p
          Jle l -> cmdJle l p
          Jl l -> cmdJl l p
          Call l -> cmdCall l p
          Ret -> cmdRet p
          Msg args -> cmdMsg args p
          End -> cmdEnd p
          Illegal str -> cmdIllegal str p

cmdMov :: Var -> Var -> Processor -> Processor
cmdMov (Lit _) _ p = cmdIllegal "mov: first argument is not a register" p
cmdMov (Reg r) (Lit n) p = p {programCounter = programCounter p + 1, registers = M.insert r n $ registers p}
cmdMov (Reg r) (Reg nreg) p = case M.lookup nreg (registers p) of
  Just nregValue -> cmdMov (Reg r) (Lit nregValue) p
  Nothing -> cmdIllegal "mov: register is not initialized" p

cmdInc :: Var -> Processor -> Processor
cmdInc (Lit _) p = cmdIllegal "inc: first argument is not a register" p
cmdInc (Reg r) p = p {programCounter = programCounter p + 1, registers = M.adjust (+ 1) r $ registers p}

cmdDec :: Var -> Processor -> Processor
cmdDec (Lit _) p = cmdIllegal "dec: first argument is not a register" p
cmdDec (Reg r) p = p {programCounter = programCounter p + 1, registers = M.adjust (+ (-1)) r $ registers p}

cmdAdd :: Var -> Var -> Processor -> Processor
cmdAdd = cmdArithm (+) "add"

cmdSub :: Var -> Var -> Processor -> Processor
cmdSub = cmdArithm (-) "sub"

cmdMul :: Var -> Var -> Processor -> Processor
cmdMul = cmdArithm (*) "mul"

cmdDiv :: Var -> Var -> Processor -> Processor
cmdDiv = cmdArithm div "div"

cmdArithm :: (Int -> Int -> Int) -> String -> Var -> Var -> Processor -> Processor
cmdArithm _ name (Lit _) _ p = cmdIllegal (name ++ ": first argument is not a register") p
cmdArithm f name (Reg r) (Lit n) p = case M.lookup r (registers p) of
  Just rValue -> p {programCounter = programCounter p + 1, registers = M.insert r (f rValue n) $ registers p}
  Nothing -> cmdIllegal (name ++ ": first register is not initialized") p
cmdArithm f name (Reg r) (Reg nreg) p = case M.lookup nreg (registers p) of
  Just nregValue -> cmdArithm f name (Reg r) (Lit nregValue) p
  Nothing -> cmdIllegal (name ++ ": second register is not initialized") p

cmdCmp :: Var -> Var -> Processor -> Processor
cmdCmp (Lit a) (Lit b) p
  | a < b = p {programCounter = programCounter p + 1, status = StatusLess}
  | a == b = p {programCounter = programCounter p + 1, status = StatusEqual}
  | otherwise = p {programCounter = programCounter p + 1, status = StatusGreater}
cmdCmp (Reg a) b p = case M.lookup a (registers p) of
  Just aValue -> cmdCmp (Lit aValue) b p
  Nothing -> cmdIllegal "cmp: first register is not initialized" p
cmdCmp a (Reg b) p = case M.lookup b (registers p) of
  Just bValue -> cmdCmp a (Lit bValue) p
  Nothing -> cmdIllegal "cmp: second register is not initialized" p

cmdJmp :: Label -> Processor -> Processor
cmdJmp l p = case M.lookup l (labels p) of
  Just pc -> p {programCounter = pc, status = StatusUndefined}
  Nothing -> cmdIllegal "jmp: no such label" p

cmdJne :: Label -> Processor -> Processor
cmdJne l p
  | status p == StatusUndefined = cmdIllegal "jne: cmp status is undefined" p
  | status p == StatusEqual = p {programCounter = programCounter p + 1}
  | otherwise = cmdJmp l p

cmdJe :: Label -> Processor -> Processor
cmdJe l p
  | status p == StatusUndefined = cmdIllegal "je: cmp status is undefined" p
  | status p == StatusEqual = cmdJmp l p
  | otherwise = p {programCounter = programCounter p + 1}

cmdJge :: Label -> Processor -> Processor
cmdJge l p
  | status p == StatusUndefined = cmdIllegal "jge: cmp status is undefined" p
  | status p == StatusLess = p {programCounter = programCounter p + 1}
  | otherwise = cmdJmp l p

cmdJg :: Label -> Processor -> Processor
cmdJg l p
  | status p == StatusUndefined = cmdIllegal "je: cmp status is undefined" p
  | status p == StatusGreater = cmdJmp l p
  | otherwise = p {programCounter = programCounter p + 1}

cmdJle :: Label -> Processor -> Processor
cmdJle l p
  | status p == StatusUndefined = cmdIllegal "jge: cmp status is undefined" p
  | status p == StatusGreater = p {programCounter = programCounter p + 1}
  | otherwise = cmdJmp l p

cmdJl :: Label -> Processor -> Processor
cmdJl l p
  | status p == StatusUndefined = cmdIllegal "je: cmp status is undefined" p
  | status p == StatusLess = cmdJmp l p
  | otherwise = p {programCounter = programCounter p + 1}

cmdCall :: Label -> Processor -> Processor
cmdCall l p
  | M.notMember l $ labels p = cmdIllegal "call: no such label" p
  | otherwise = cmdJmp l p {stack = (programCounter p + 1) : stack p}

cmdRet :: Processor -> Processor
cmdRet p = case stack p of
  [] -> cmdIllegal "ret: not inside a function" p
  (h : t) -> p {programCounter = h, stack = t}

cmdMsg :: [String] -> Processor -> Processor
cmdMsg args p = case errors of
  [] -> p {output = Just (unwords strs)}
  (err : _) -> cmdIllegal err p
  where
    processedArgs = map (processMsgArg p) args
    strs = map fst processedArgs
    errors = map snd processedArgs

processMsgArg :: Processor -> String -> (String, String)
processMsgArg p str
  | '\'' `elem` str = case str of
      ('\'' : rest) -> case reverse rest of
        ('\'' : rst) | '\'' `notElem` rst -> (reverse rst, "")
        _invalidString -> ("", "msg: invalid string argument")
      _invalidString -> ("", "msg: invalid string argument")
  | otherwise = case M.lookup str (registers p) of
      Just value -> (show value, "")
      Nothing -> ("", "msg: invalid argument: register not initialized")

cmdEnd :: Processor -> Processor
cmdEnd p = p {programCounter = maxInt, ended = True}

cmdIllegal :: String -> Processor -> Processor
cmdIllegal str p = p {programCounter = maxInt, output = Just str}

cleanup :: String -> [String]
cleanup = filter (not . null) . map (unwords . words . takeWhile (/= ';')) . lines

parseProgram :: String -> ([Cmd], Labels)
parseProgram input =
  let lst = zipWith (curry parseCmdAndLabel) [0 ..] (combineAdjacent $ cleanup input)
   in (map fst lst, M.fromList $ mapMaybe snd lst)

containsLabel :: String -> Bool
containsLabel = not . any isDelimiter . takeWhile (/= ':')
  where
    isDelimiter c = isSpace c || c == ','

containsOnlyLabel :: String -> Bool
containsOnlyLabel str = containsLabel str && null (dropWhile (/= ':') str)

combineAdjacent :: [String] -> [String]
combineAdjacent [] = []
combineAdjacent [x] = [x]
combineAdjacent (x : y : xs)
  | containsOnlyLabel x = (x ++ y) : combineAdjacent xs
  | otherwise = x : combineAdjacent (y : xs)

parseCmdAndLabel :: (Int, String) -> (Cmd, Maybe (Label, Int))
parseCmdAndLabel (i, line)
  | containsLabel line = case splitOn ":" line of
      (before : after) -> (parseCmd $ dropWhile isSpace $ concat after, Just (before, i))
      _noLabel -> (parseCmd line, Nothing)
  | otherwise = (parseCmd line, Nothing)

parseCmd :: String -> Cmd
parseCmd str = parseCmd' $ map (unwords . words) $ splitOn "," str

parseCmd' :: [String] -> Cmd
parseCmd' [] = Illegal "internal error"
parseCmd' (cmd : args) = case args of
  [] -> case cmd of
    "ret" -> Ret
    "end" -> End
    _ -> Illegal cmd
  [a] -> case cmd of
    "inc" -> Inc (Reg a)
    "dec" -> Dec (Reg a)
    "jmp" -> Jmp a
    "jne" -> Jne a
    "je" -> Je a
    "jge" -> Jge a
    "jg" -> Jg a
    "jle" -> Jle a
    "jl" -> Jl a
    "call" -> Call a
    _ -> Illegal $ unwords $ cmd : args
  [a, b] -> case cmd of
    "mov" -> Mov (pv a) (pv b)
    "add" -> Add (pv a) (pv b)
    "sub" -> Sub (pv a) (pv b)
    "mul" -> Mul (pv a) (pv b)
    "div" -> Div (pv a) (pv b)
    "cmp" -> Cmp (pv a) (pv b)
    _ -> Illegal $ unwords $ cmd : args
  arr -> case cmd of
    "msg" -> Msg arr
    _ -> Illegal $ unwords $ cmd : args
  where
    pv name =
      let num = (readMaybe name :: Maybe Int)
       in case num of
            Just n -> Lit n
            Nothing -> Reg name
