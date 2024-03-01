module Main where

import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust, isJust)
import Text.Read (readMaybe)

main :: IO ()
main = interact (show . interpret)

-- main = interact (show . parseProgram)

interpret :: String -> Maybe String
interpret = output . execProgram . uncurry initProcessor . parseProgram

data Status = Undefined | L | LEQ | EQ | NEQ | GEQ | G

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
  Illegal :: Cmd
  deriving (Show, Eq)

data Processor = Processor
  { programCounter :: !Int,
    status :: !Status,
    registers :: !Registers,
    stack :: ![Int],
    labels :: !(M.Map Label Int),
    commands :: ![Cmd],
    output :: !(Maybe String)
  }

initProcessor :: [Cmd] -> Labels -> Processor
initProcessor commands labels =
  Processor
    { programCounter = 0,
      status = Undefined,
      registers = M.empty,
      stack = [],
      labels = labels,
      commands = commands,
      output = Nothing
    }

execProgram :: Processor -> Processor
execProgram p =
  let cmd = (commands p) !! (programCounter p)
   in case cmd of
        _ -> p

cleanup :: String -> [String]
cleanup = filter (\line -> length line > 0) . map unwords . map words . map (takeWhile (/= ';')) . lines

parseProgram :: String -> ([Cmd], Labels)
parseProgram input =
  let lst = map parseCmdAndLabel $ zip [0 ..] $ combineAdjacent $ cleanup input
   in (map fst lst, M.fromList $ map fromJust $ filter isJust $ map snd lst)

endsWithColon :: String -> Bool
endsWithColon str = last str == ':'

combineAdjacent :: [String] -> [String]
combineAdjacent [] = []
combineAdjacent [x] = [x]
combineAdjacent (x : y : xs)
  | endsWithColon x = (x ++ y) : combineAdjacent xs
  | otherwise = x : combineAdjacent (y : xs)

parseCmdAndLabel :: (Int, String) -> (Cmd, Maybe (Label, Int))
parseCmdAndLabel (i, line)
  | ':' `elem` line =
      let lst = splitOn ":" line
       in (parseCmd $ lst !! 1, Just (head lst, i))
  | otherwise = (parseCmd line, Nothing)

parseCmd :: String -> Cmd
parseCmd str =
  let (cmd : args') = words str
   in let args = if length args' == 0 then args' else map (unwords . words) $ splitOn "," $ unwords args'
       in case args of
            [] -> case cmd of
              "ret" -> Ret
              "end" -> End
              _ -> Illegal
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
              _ -> Illegal
            [a, b] -> case cmd of
              "mov" -> Mov (pv a) (pv b)
              "add" -> Add (pv a) (pv b)
              "sub" -> Sub (pv a) (pv b)
              "mul" -> Mul (pv a) (pv b)
              "div" -> Div (pv a) (pv b)
              "cmp" -> Cmp (pv a) (pv b)
              _ -> Illegal
            arr -> case cmd of
              "msg" -> Msg arr
              _ -> Illegal
  where
    pv name =
      let num = (readMaybe name :: Maybe Int)
       in case num of
            Just n -> Lit n
            Nothing -> Reg name
