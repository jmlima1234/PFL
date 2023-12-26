-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

import Data.List (intercalate)
import qualified Data.List as List

import Data.Function (on)

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

type Stack = [Integer]

type State = [(String, Integer)]

createEmptyStack :: Stack
createEmptyStack = [] -- TODO, Uncomment the function signature after defining Stack

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map show stack) -- TODO, Uncomment all the other function type declarations as you implement them

createEmptyState :: State
createEmptyState = [] -- TODO, Uncomment the function signature after defining State

state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ show val | (var, val) <- sortBy (compare `on` fst) state]

-- run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined -- TODO

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

