-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

import Data.List (sortOn, intercalate)
import qualified Data.List as List
import Data.List (sort)
import Data.Function (on)
import Data.List (find)


-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

type Stack = [Inst]

data Value = IntValue Integer | BoolValue Bool deriving Show
type State = [(String, Value)]

createEmptyStack :: Stack
createEmptyStack = [] -- TODO, Uncomment the function signature after defining Stack

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map instToStr stack)
  where
    instToStr :: Inst -> String
    instToStr (Push n) = boolToStr n
    instToStr Tru = "True"
    instToStr Fals = "False"
    instToStr inst = show inst

    boolToStr :: Integer -> String
    boolToStr n = show n

createEmptyState :: State
createEmptyState = [] -- TODO, Uncomment the function signature after defining State

state2Str :: State -> String
state2Str state = intercalate "," (sortOn id (map (\(var, val) -> var ++ "=" ++ valueToStr val) state))
  where
    valueToStr :: Value -> String
    valueToStr (IntValue n) = show n
    valueToStr (BoolValue b) = if b then "True" else "False"

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)

-- Push
run ((Push n):code, stack, state) = run (code, (Push n):stack, state)

-- Add
run (Add:code, (Push n1):(Push n2):stack, state) = run (code, (Push (n1 + n2)):stack, state)
run (Add:code, stack, state) = error "Run-time error"

-- Mult
run (Mult:code, (Push n1):(Push n2):stack, state) = run (code, (Push (n1 * n2)):stack, state)
run (Mult:code, stack, state) = error "Run-time error"

-- Sub
run (Sub:code, (Push n1):(Push n2):stack, state) = run (code, (Push (n1 - n2)):stack, state)
run (Sub:code, stack, state) = error "Run-time error"

-- Tru
run (Tru:code, stack, state) = run (code, (Tru):stack, state)

-- Fals
run (Fals:code, stack, state) = run (code, (Fals):stack, state)

-- Equ
run (Equ:code, (Tru):(Tru):stack, state) = run (code, (Tru):stack, state)
run (Equ:code, (Fals):(Fals):stack, state) = run (code, (Tru):stack, state)
run (Equ:code, (Tru):(Fals):stack, state) = run (code, (Fals):stack, state)
run (Equ:code, (Fals):(Tru):stack, state) = run (code, (Fals):stack, state)
run (Equ:code, (Push n1):(Push n2):stack, state) = run (code, (if n1 == n2 then Tru else Fals):stack, state)
run (Equ:code, stack, state) = error "Run-time error"

-- Le
run (Le:code, (Push n1):(Push n2):stack, state) = run (code, (if n1 <= n2 then Tru else Fals):stack, state)
run (Le:code, stack, state) = error "Run-time error"

-- And
run (And:code, (Tru):(Tru):stack, state) = run (code, (Tru):stack, state)
run (And:code, (Fals):(Tru):stack, state) = run (code, (Fals):stack, state)
run (And:code, (Tru):(Fals):stack, state) = run (code, (Fals):stack, state)
run (And:code, (Fals):(Fals):stack, state) = run (code, (Fals):stack, state)
run (And:code, stack, state) = error "Run-time error"

-- Neg
run (Neg:code, (Tru):stack, state) = run (code, (Fals):stack, state)
run (Neg:code, (Fals):stack, state) = run (code, (Tru):stack, state)
run (Neg:code, stack, state) = error "Run-time error"

-- noop
run (Noop:code, stack, state) = run (code, stack, state)

-- Fetch
run ((Fetch var):code, stack, state) = 
  case lookup var state of
    Just (IntValue val) -> run (code, (Push val):stack, state)
    Just (BoolValue val) -> run (code, (if val then Tru else Fals):stack, state)
    Nothing -> error "Run-time error"

-- Store
run ((Store var):code, (Push val):stack, state) = run (code, stack, (var, IntValue val):removeOldAssignments var state)
run ((Store var):code, Tru:stack, state) = run (code, stack, (var, BoolValue True):removeOldAssignments var state)
run ((Store var):code, Fals:stack, state) = run (code, stack, (var, BoolValue False):removeOldAssignments var state)
run ((Store var):code, stack, state) = error "Run-time error"

-- Branch
run ((Branch code1 code2):code, (Tru):stack, state) = run (code1 ++ code, stack, state)
run ((Branch code1 code2):code, (Fals):stack, state) = run (code2 ++ code, stack, state)
run ((Branch code1 code2):code, stack, state) = error "Run-time error"

-- Loop
run ((Loop code1 code2):code, stack, state) = run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ code, stack, state)



removeOldAssignments :: String -> State -> State
removeOldAssignments var state = filter ((/= var) . fst) state

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program
data Aexp = 
    Var String
  | Num Integer
  | Aexp :+: Aexp
  | Aexp :-: Aexp
  | Aexp :*: Aexp
  deriving Show

data Bexp =
    Bexp :&: Bexp
  | Bexp :|: Bexp
  | Not Bexp
  | Aexp :==: Aexp
  | Aexp :<=: Aexp
  | BTrue
  | BFalse
  deriving Show

data Stm =
    String :=: Aexp
  | Seq Stm Stm
  | While Bexp Stm
  deriving Show



testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"


-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO
