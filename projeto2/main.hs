-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

-- Part 1

import Data.List (sortOn, intercalate)
import qualified Data.List as List
import Data.List

import Data.List (sort)
import Data.Function (on)
import Data.List (find)
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Control.Applicative ((<*), (*>), (<|>))
import Control.Applicative (many)
import Data.Functor.Identity (Identity)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, ParseError)
import Text.Parsec.String (parseFromFile)
import Text.Parsec (parse, ParseError, endBy)





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

myIdentifier :: Parser String
myIdentifier = Token.identifier lexer

myInteger :: Parser Integer
myInteger = Token.integer lexer

aexp :: Parser Aexp
aexp = buildExpressionParser aexpOperators aexpTerm

aexpTerm :: Parser Aexp
aexpTerm = parens lexer aexp
       <|> Var <$> myIdentifier
       <|> Num <$> myInteger

aexpOperators :: OperatorTable String () Identity Aexp
aexpOperators = [ [Infix (reservedOp lexer "*" >> return (:*:)) AssocLeft]
                , [Infix (reservedOp lexer "-" >> return (:-:)) AssocLeft]
                , [Infix (reservedOp lexer "+" >> return (:+:)) AssocLeft]
                ]

bexp :: Parser Bexp
bexp = buildExpressionParser bexpOperators bexpTerm

bexpTerm :: Parser Bexp
bexpTerm = parens lexer bexp
       <|> BVar <$> myIdentifier
       <|> BConst <$> myInteger
       <|> BoolConst <$> (True <$ reserved lexer "true" <|> False <$ reserved lexer "false")
       <|> Not <$ reservedOp lexer "¬" <*> bexpTerm

bexpOperators :: OperatorTable String () Identity Bexp
bexpOperators = [ [Prefix (reservedOp lexer "not" >> return Not)]
                , [Infix (reservedOp lexer "and" >> return (:&:)) AssocLeft]
                , [Infix (reservedOp lexer "==" >> return (:==:)) AssocLeft]
                , [Infix (reservedOp lexer "<=" >> return (:<=:)) AssocLeft]
                ]

