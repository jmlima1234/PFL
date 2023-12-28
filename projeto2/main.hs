-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

import Data.List (sortOn, intercalate)
import qualified Data.List as List
import Data.List (sort)
import Data.Function (on)

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

type Stack = [Inst]

type State = [(String, Integer)]

createEmptyStack :: Stack
createEmptyStack = [] -- TODO, Uncomment the function signature after defining Stack

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map instToStr (reverse stack))
  where
    instToStr :: Inst -> String
    instToStr (Push n) = show n
    instToStr Tru = "True"
    instToStr Fals = "False"
    instToStr inst = show inst

createEmptyState :: State
createEmptyState = [] -- TODO, Uncomment the function signature after defining State

state2Str :: State -> String
state2Str state = intercalate "," (sortOn id (map (\(var, val) -> var ++ "=" ++ show val) state))

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push n):code, stack, state) = run (code, (Push n):stack, state)
run (Add:code, (Push n1):(Push n2):stack, state) = run (code, (Push (n1 + n2)):stack, state)
run (Mult:code, (Push n1):(Push n2):stack, state) = run (code, (Push (n1 * n2)):stack, state)
run (Sub:code, (Push n1):(Push n2):stack, state) = run (code, (Push (n1 - n2)):stack, state)
run (Tru:code, stack, state) = run (code, (Tru):stack, state)
run (Fals:code, stack, state) = run (code, (Fals):stack, state)
run (Equ:code, (Push n1):(Push n2):stack, state) = run (code, (if n1 == n2 then Tru else Fals):stack, state)
run (Le:code, (Push n1):(Push n2):stack, state) = run (code, (if n1 <= n2 then Tru else Fals):stack, state)
run (And:code, (Tru):(Tru):stack, state) = run (code, (Tru):stack, state)
run (And:code, (Fals):(Tru):stack, state) = run (code, (Fals):stack, state)
run (And:code, (Tru):(Fals):stack, state) = run (code, (Fals):stack, state)
run (And:code, (Fals):(Fals):stack, state) = run (code, (Fals):stack, state)
run (And:code, stack, state) = error "Run-time error"
run (Neg:code, (Tru):stack, state) = run (code, (Fals):stack, state)
run (Neg:code, (Fals):stack, state) = run (code, (Tru):stack, state)
run ((Fetch var):code, stack, state) = run (code, (Push (fetch var state)):stack, state)
run ((Store var):code, (Push n):stack, state) = run (code, stack, (store var n state))
run (Noop:code, stack, state) = run (code, stack, state)
run ((Branch code1 code2):code, (Tru):stack, state) = run (code1 ++ code, stack, state)
run ((Branch code1 code2):code, (Fals):stack, state) = run (code2 ++ code, stack, state)
run ((Loop code1 code2):code, (Tru):stack, state) = run (code1 ++ [Loop code1 code2] ++ code, stack, state)
run ((Loop code1 code2):code, (Fals):stack, state) = run (code, stack, state)

testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

fetch :: String -> State -> Integer
fetch var state = 
  case lookup var state of
    Nothing -> error ("Variable " ++ var ++ " not found")
    Just n  -> n

store :: String -> Integer -> State -> State
store var n state = (var, n) : filter ((/= var) . fst) state


-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
-- compA (Var x) = [Fetch x]
-- compA (Num n) = [Push n]
-- compA (Add a1 a2) = compA a2 ++ compA a1 ++ [Add]
-- compA (Sub a1 a2) = compA a2 ++ compA a1 ++ [Sub]
-- compA (Mult a1 a2) = compA a2 ++ compA a1 ++ [Mult]

-- compB :: Bexp -> Code
-- compB TrueB = [Tru]
-- compB FalseB = [Fals]
-- compB (Not b) = compB b ++ [Neg]
-- compB (Equal a1 a2) = compA a2 ++ compA a1 ++ [Equ]

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

