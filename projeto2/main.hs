-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

import Data.List
import Data.Function (on)
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Control.Applicative ((<*), (*>), (<|>))
import Data.Functor.Identity (Identity)


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
    BVar String 
  | BConst Integer 
  | BoolConst Bool
  | Bexp :&: Bexp
  | Bexp :|: Bexp
  | Not Bexp
  | Bexp :==: Bexp
  | Bexp :<=: Bexp
  | BTrue
  | BFalse
  | EqAexp Aexp Aexp
  deriving Show

data Stm =
    String :=: Aexp
  | Seq [Stm] 
  | If [Bexp] [Stm] [Stm] 
  | While [Bexp] [Stm]
  deriving Show

data Token = PlusTok | MinusTok | TimesTok | DivTok | OpenTok | CloseTok | IntTok Integer | VarTok String | AssignTok | WhileTok | DoTok |
            TrueTok | FalseTok | AndTok | OrTok | NotTok | EqTok | LtTok | IfTok | ThenTok | IntEqTok | BoolEqTok|  ElseTok | SemicolonTok deriving (Show)

compA :: Aexp -> Code
compA (Var x) = [Fetch x]
compA (Num n) = [Push n]
compA (a1 :+: a2) = compA a2 ++ compA a1 ++ [Add]
compA (a1 :-: a2) = compA a2 ++ compA a1 ++ [Sub]
compA (a1 :*: a2) = compA a2 ++ compA a1 ++ [Mult]

-- compB 
compB :: [Bexp] -> Code
compB [] = []
compB (x:xs) = 
    case x of
        BoolConst b -> (if b then [Tru] else [Fals]) ++ compB xs
        BConst n -> [Push n] ++ compB xs
        BVar n -> [Fetch n] ++ compB xs
        BTrue -> [Tru] ++ compB xs
        BFalse -> [Fals] ++ compB xs
        x1 :&: x2 -> compB [x1] ++ compB [x2] ++ [And] ++ compB xs
        x1 :|: x2 -> compB [x1] ++ compB [x2] ++ [Branch [Tru] [Fals]] ++ compB xs
        Not x -> compB [x] ++ [Neg] ++ compB xs
        x1 :==: x2 -> compB [x1] ++ compB [x2] ++ [Equ] ++ compB xs
        x1 :<=: x2 -> compB [x1] ++ compB [x2] ++ [Le] ++ compB xs
        EqAexp x1 x2 -> compA x1 ++ compA x2 ++ [Equ] ++ compB xs

-- compile 
compile :: [Stm] -> Code
compile [] = []
compile ((x :=: a):xs) = compA a ++ [Store x] ++ compile xs
compile ((Seq stms):xs) = compile stms ++ compile xs
compile ((If x stm1 stm2):xs) = compB x ++ [Branch (compile stm1) (compile stm2)] ++ compile xs
compile ((While x stm):xs) = [Loop (compB x) (compile stm)] ++ compile xs

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef

parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens =
    case tokens of
        (IntTok n : restTokens) -> Just (Num n, restTokens)
        (VarTok v : restTokens) -> Just (Var v, restTokens)
        _ -> Nothing
    
parseAexpOrParent :: [Token] -> Maybe (Aexp, [Token])
parseAexpOrParent (OpenTok : restofTokens) =
    case parseAddOrSubMultOrAexpOrParent restofTokens of
      Just (expr, (CloseTok : restofTokens2)) ->
          Just (expr,restofTokens2)
      Just _ -> Nothing
      Nothing -> Nothing
parseAexpOrParent tokens = parseAexp tokens

parseMultOrAexpOrParent :: [Token] -> Maybe (Aexp, [Token])
parseMultOrAexpOrParent tokens =
    case parseAexpOrParent tokens of 
        Just (expr, (TimesTok : restofTokens)) ->
            case parseMultOrAexpOrParent restofTokens of
                Just (expr2, restofTokens2) -> Just (expr :*: expr2, restofTokens2)
                Nothing -> Nothing
        result -> result

parseAddOrSubMultOrAexpOrParent :: [Token] -> Maybe (Aexp, [Token])
parseAddOrSubMultOrAexpOrParent tokens = 
    case parseMultOrAexpOrParent tokens of
      Just (expr, (PlusTok : restofTokens)) ->
        case parseAddOrSubMultOrAexpOrParent restofTokens of
          Just (expr2, restofTokens2) -> Just (expr :+: expr2, restofTokens2)
          Nothing -> Nothing
      Just (expr, (MinusTok : restofTokens)) ->
        case parseAddOrSubMultOrAexpOrParent restofTokens of
          Just (expr2,restofTokens2) -> Just (expr :-: expr2, restofTokens2)
          Nothing -> Nothing
      result -> result


-- parse :: String -> Program
-- parse str = parseProgram (lexer str)

-- parseProgram :: [Token] -> Program
-- parseProgram tokens = 
--  case tokens of
--    [] -> []
--    _ ->
--      case parseStm tokens of
--        Just (stm, restTokens) -> stm : parseProgram restTokens
--        _ -> []

-- Examples:
main :: IO ()
main = do
    ---------------------------------------------------------------------------------------
    -- PART 1 TESTS
    print $ testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
    print $ testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
    print $ testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
    print $ testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
    print $ testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
    print $ testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
    print $ testAssembler [Push (-20),Push (-21), Le] == ("True","")
    print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
    print $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
    -- If you test:
    -- testAssembler [Push 1,Push 2,And]
    -- You should get an exception with the string: "Run-time error"
    -- If you test:
    -- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
    -- You should get an exception with the string: "Run-time error"
    ---------------------------------------------------------------------------------------
    -- PART 2 TESTS
    print $ testParser "x := 5; x := x - 1;" == ("","x=4")
    print $ testParser "x := 0 - 2;" == ("","x=-2")
    print $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
    print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
    print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
    print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
    print $ testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
    print $ testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
    print $ testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
    print $ testParser "if (1 + 1 == 2 = 1 + 2 == 3) then x := 1; else x := 2;" == ("","x=1")
    print $ testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
    print $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
    print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
    print $ testParser "x := 5; x := x - 1;" == ("","x=4")
    print $ testParser "x := 0 - 2;" == ("","x=-2")
    print $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
    print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
    print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
    print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
    print $ testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
    print $ testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
    print $ testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
    print $ testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
    print $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
    print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
    ---------------------------------------------------------------------------------------