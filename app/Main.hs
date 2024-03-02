module Main where

-- I will port to Data.Text after I get this program working
-- import Data.Text qualified as T

import Data.Char (isDigit, isSpace)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import GHC.Base (maxInt)

main :: IO ()
main = interact (show . interpret)

interpret :: String -> Maybe String
interpret p = if ended endP then output endP else Nothing
  where
    endP = (execProgram . uncurry initProcessor . parseProgram) p

data Status = StatusUndefined | StatusLess | StatusEqual | StatusGreater deriving (Show, Eq)

type Registers = M.Map String Int

type Label = String

type Labels = M.Map Label Int

data Var where
  Lit :: Int -> Var
  Reg :: String -> Var
  deriving (Show, Eq)

data CmdArg where
  CmdArgInt :: !Int -> CmdArg
  CmdArgString :: !String -> CmdArg
  CmdArgRegOrLabel :: !String -> CmdArg
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
  Msg :: [CmdArg] -> Cmd
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
  deriving (Show)

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
        Just cmd -> execProgram (execCommand p cmd)

execCommand :: Processor -> Cmd -> Processor
execCommand p cmd = case cmd of
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
  | otherwise = cmdJmp l (p {stack = (programCounter p + 1) : stack p})

cmdRet :: Processor -> Processor
cmdRet p = case stack p of
  [] -> cmdIllegal "ret: not inside a function" p
  (h : t) -> p {programCounter = h, stack = t}

cmdMsg :: [CmdArg] -> Processor -> Processor
cmdMsg args p = case errors of
  errs | null errs || all null errs -> p {programCounter = programCounter p + 1, output = Just (concat strs)}
  (err : _) -> cmdIllegal err p
  [] -> p {programCounter = programCounter p + 1, output = Just ""}
  where
    processedArgs = map (processCmdMsgArg p) args
    strs = map fst processedArgs
    errors = map snd processedArgs

processCmdMsgArg :: Processor -> CmdArg -> (String, String)
processCmdMsgArg _ (CmdArgInt num) = (show num, "")
processCmdMsgArg _ (CmdArgString arg) = (arg, "")
processCmdMsgArg p (CmdArgRegOrLabel reg) = case M.lookup reg (registers p) of
  Just num -> (show num, "")
  Nothing -> ("", "msg: register not initialized")

cmdEnd :: Processor -> Processor
cmdEnd p = p {programCounter = maxInt, ended = True}

cmdIllegal :: String -> Processor -> Processor
cmdIllegal str p = p {programCounter = maxInt, output = Just str}

parseProgram :: String -> ([Cmd], Labels)
parseProgram input =
  let lst = zipWith (curry parseCmdAndLabel) [0 ..] (combineAdjacent $ cleanup input)
   in (map fst lst, M.fromList $ mapMaybe snd lst)

cleanup :: String -> [String]
cleanup = filter (not . null) . map (reverse . dropWhile isSpace . reverse . dropWhile isSpace . takeWhile (/= ';')) . lines

combineAdjacent :: [String] -> [String]
combineAdjacent [] = []
combineAdjacent [x] = [x]
combineAdjacent (x : y : xs)
  | containsOnlyLabel x = (x ++ y) : combineAdjacent xs
  | otherwise = x : combineAdjacent (y : xs)

containsOnlyLabel :: String -> Bool
containsOnlyLabel str = containsLabel str && dropWhile (/= ':') str == ":"

containsLabel :: String -> Bool
containsLabel str = ':' `elem` str && not (any isDelimiter $ takeWhile (/= ':') str)
  where
    isDelimiter c = isSpace c || c == ','

parseCmdAndLabel :: (Int, String) -> (Cmd, Maybe (Label, Int))
parseCmdAndLabel (i, line)
  | containsLabel line = case splitOn ":" line of
      (before : after) -> (parseCmd $ intercalate ":" after, Just (before, i))
      _noLabel -> (parseCmd line, Nothing)
  | otherwise = (parseCmd line, Nothing)

parseCmd :: String -> Cmd
parseCmd str = case maybeArgs of
  Nothing -> Illegal $ "parse error: invalid arguments: " ++ str
  Just args -> parseCmd' cmd args
  where
    preTrimmed = dropWhile isSpace str
    cmd = takeWhile (not . isSpace) preTrimmed
    maybeArgs = parseCmdArgs $ dropWhile isSpace $ dropWhile (not . isSpace) preTrimmed

parseCmd' :: String -> [CmdArg] -> Cmd
parseCmd' cmd args = case args of
  [] -> case cmd of
    "ret" -> Ret
    "end" -> End
    _ -> Illegal $ "parse error on command " ++ cmd ++ " with args " ++ show args
  [a] -> case a of
    CmdArgRegOrLabel rl -> case cmd of
      "inc" -> Inc (Reg rl)
      "dec" -> Dec (Reg rl)
      "jmp" -> Jmp rl
      "jne" -> Jne rl
      "je" -> Je rl
      "jge" -> Jge rl
      "jg" -> Jg rl
      "jle" -> Jle rl
      "jl" -> Jl rl
      "call" -> Call rl
      "msg" -> Msg args
      _ -> Illegal $ "parse error on command " ++ cmd ++ " with args " ++ show args
    CmdArgInt _ -> case cmd of
      "msg" -> Msg args
      _ -> Illegal $ "parse error on command " ++ cmd ++ " with args " ++ show args
    CmdArgString _ -> case cmd of
      "msg" -> Msg args
      _ -> Illegal $ "parse error on command " ++ cmd ++ " with args " ++ show args
  [a, b] -> case a of
    CmdArgRegOrLabel rla -> case b of
      CmdArgRegOrLabel rlb -> case cmd of
        "mov" -> Mov (Reg rla) (Reg rlb)
        "add" -> Add (Reg rla) (Reg rlb)
        "sub" -> Sub (Reg rla) (Reg rlb)
        "mul" -> Mul (Reg rla) (Reg rlb)
        "div" -> Div (Reg rla) (Reg rlb)
        "cmp" -> Cmp (Reg rla) (Reg rlb)
        "msg" -> Msg args
        _ -> Illegal $ "parse error on command " ++ cmd ++ " with args " ++ show args
      CmdArgInt num -> case cmd of
        "mov" -> Mov (Reg rla) (Lit num)
        "add" -> Add (Reg rla) (Lit num)
        "sub" -> Sub (Reg rla) (Lit num)
        "mul" -> Mul (Reg rla) (Lit num)
        "div" -> Div (Reg rla) (Lit num)
        "cmp" -> Cmp (Reg rla) (Lit num)
        "msg" -> Msg args
        _ -> Illegal $ "parse error on command " ++ cmd ++ " with args " ++ show args
      CmdArgString _ -> case cmd of
        "msg" -> Msg args
        _ -> Illegal $ "parse error on command " ++ cmd ++ " with args " ++ show args
    CmdArgInt numa -> case b of
      CmdArgRegOrLabel rl -> case cmd of
        "cmp" -> Cmp (Lit numa) (Reg rl)
        "msg" -> Msg args
        _ -> Illegal $ "parse error on command " ++ cmd ++ " with args " ++ show args
      CmdArgInt num -> case cmd of
        "cmp" -> Cmp (Lit numa) (Lit num)
        "msg" -> Msg args
        _ -> Illegal $ "parse error on command " ++ cmd ++ " with args " ++ show args
      CmdArgString _ -> case cmd of
        "msg" -> Msg args
        _ -> Illegal $ "parse error on command " ++ cmd ++ " with args " ++ show args
    CmdArgString _ -> case cmd of
      "msg" -> Msg args
      _ -> Illegal $ "parse error on command " ++ cmd ++ " with args " ++ show args
  args' -> case cmd of
    "msg" -> Msg args'
    _ -> Illegal $ "parse error on command " ++ cmd ++ " with args " ++ show args

data ParserState = ParsingWhitespace | ParsingRegName | ParsingString | ParsingInt deriving (Show)

data CmdArgParser = CmdArgParser
  { string :: !String,
    state :: !ParserState,
    argBuffer :: !String,
    args :: ![CmdArg],
    valid :: !Bool
  }
  deriving (Show)

parseCmdArgs :: String -> Maybe [CmdArg]
parseCmdArgs str = if valid p then Just (args p) else Nothing
  where
    p = newP {args = reverse $ args newP}
    newP = parseCmdArgs' initParser
    initParser =
      CmdArgParser
        { string = str,
          state = ParsingWhitespace,
          argBuffer = "",
          args = [],
          valid = False
        }

parseCmdArgs' :: CmdArgParser -> CmdArgParser
parseCmdArgs' p | valid p = p
parseCmdArgs' p = case state p of
  ParsingWhitespace -> case string p of
    [] -> p {valid = True}
    (x : rest) -> case x of
      sym | isDigit sym -> parseCmdArgs' $ p {state = ParsingInt}
      sym | isSpace sym || sym == ',' -> parseCmdArgs' $ p {string = rest}
      '\'' -> parseCmdArgs' $ p {string = rest, state = ParsingString}
      _startOfRegisterName -> parseCmdArgs' $ p {state = ParsingRegName}
  ParsingRegName -> case string p of
    [] ->
      p
        { args = CmdArgRegOrLabel (reverse $ argBuffer p) : args p,
          argBuffer = "",
          valid = True
        }
    (x : rest) -> case x of
      sym
        | isSpace sym || sym == ',' ->
            parseCmdArgs' $
              p
                { string = rest,
                  state = ParsingWhitespace,
                  args = CmdArgRegOrLabel (reverse (argBuffer p)) : args p,
                  argBuffer = ""
                }
      _notWhitespace -> parseCmdArgs' $ p {string = rest, argBuffer = x : argBuffer p}
  ParsingString -> case string p of
    [] -> p {valid = False}
    (x : rest) -> case x of
      '\'' ->
        parseCmdArgs' $
          p
            { string = rest,
              state = ParsingWhitespace,
              args = CmdArgString (reverse (argBuffer p)) : args p,
              argBuffer = ""
            }
      _anythingElse -> parseCmdArgs' $ p {string = rest, argBuffer = x : argBuffer p}
  ParsingInt -> case string p of
    [] ->
      p
        { args = CmdArgInt ((read $ reverse $ argBuffer p) :: Int) : args p,
          argBuffer = "",
          valid = True
        }
    (x : rest) -> parseCmdArgs' $ case x of
      sym
        | sym == ',' || isSpace sym ->
            p
              { string = rest,
                state = ParsingWhitespace,
                args = CmdArgInt ((read $ reverse (argBuffer p)) :: Int) : args p,
                argBuffer = ""
              }
      sym
        | isDigit sym ->
            parseCmdArgs' $
              p
                { string = rest,
                  argBuffer = x : argBuffer p
                }
      _invalidCharacter -> p {valid = False}
