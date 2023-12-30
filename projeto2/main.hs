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

compA :: Aexp -> Code
compA (Var x) = [Fetch x]
compA (Num n) = [Push n]
compA (a1 :+: a2) = compA a2 ++ compA a1 ++ [Add]
compA (a1 :-: a2) = compA a2 ++ compA a1 ++ [Sub]
compA (a1 :*: a2) = compA a2 ++ compA a1 ++ [Mult]

compB :: Bexp -> Code
compB (b1 :&: b2) = compB b2 ++ compB b1 ++ [And]
compB (b1 :|: b2) = compB b1 ++ [Neg] ++ compB b2 ++ [Neg, And, Neg]
compB (Not b) = compB b ++ [Neg]
compB (a1 :==: a2) = compA a2 ++ compA a1 ++ [Equ]
compB (a1 :<=: a2) = compA a2 ++ compA a1 ++ [Le]
compB BTrue = [Tru]
compB BFalse = [Fals]

compStm :: Stm -> Code
compStm (x :=: a) = compA a ++ [Store x]
compStm (Seq s1 s2) = compStm s1 ++ compStm s2
compStm (While b s) = [Loop (compB b) (compStm s ++ [Branch [Noop] [Noop]])]

compile :: [Stm] -> Code
compile [] = []
compile (stm:stmts) = compStm stm ++ compile stmts

lexer :: String -> [String]
lexer [] = []
lexer str
    | isPrefixOf " " str = lexer (dropWhile (== ' ') str)
    | otherwise = case find (`isPrefixOf` str) delimiters of
        Just delimiter -> delimiter : lexer (drop (length delimiter) str)
        Nothing -> let (token, rest) = break (`elem` operatorChars) str
                   in if not (null token) then token : lexer rest else lexer rest
  where
    delimiters = ["+","-","*","<=","==",":=", "=","<","(",")","{","}",";","not","and","or","if","then","else","while","do","True","False", "i", "10", "fact", "1", "do"]
    operatorChars = ' ' : nub (concat delimiters)

testProgram :: [Stm]
testProgram =
  [ Assign "x" (AConest 10)
  , Assign "y" (AMult (AVar "x") (AConst 2))
  , If (BLe (AVar "x") (AVar "y"))
    (Assign "z" (AAdd (AVar "x") (AVar "y")))
    (Assign "z" (ASub (AVar "x") (AVar "y")))
  ]

testCompile :: Code
testCompile = compile testProgram

-- parse :: String -> Program
parse = undefined -- TODO
