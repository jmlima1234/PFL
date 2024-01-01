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
import Control.Applicative (many)
import Data.Functor.Identity (Identity)
import Data.Char (isDigit)

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
  | Aexp :==: Aexp
  | Aexp :<=: Aexp
  | BTrue
  | BFalse
  deriving Show

data Stm =
    String :=: Aexp
  | Seq [Stm] 
  | If [Bexp] [Stm] [Stm] 
  | While [Bexp] [Stm]
  | Skip
  deriving Show

data Token = PlusTok | MinusTok | TimesTok | DivTok | OpenTok | CloseTok | IntTok Integer | VarTok String | AssignTok | WhileTok | DoTok |
            TrueTok | FalseTok | AndTok | OrTok | NotTok | EqTok | LeTok | IfTok | ThenTok | IntEqTok | BoolEqTok|  ElseTok | SemicolonTok deriving (Show)

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
        a1 :==: a2 -> compA a1 ++ compA a2 ++ [Equ] ++ compB xs
        a1 :<=: a2 -> compA a1 ++ compA a2 ++ [Le] ++ compB xs

-- compile 
compile :: [Stm] -> Code
compile [] = []
compile ((x :=: a):xs) = compA a ++ [Store x] ++ compile xs
compile ((Seq stms):xs) = compile stms ++ compile xs
compile ((If x stm1 stm2):xs) = compB x ++ [Branch (compile stm1) (compile stm2)] ++ compile xs
compile ((While x stm):xs) = [Loop (compB x) (compile stm)] ++ compile xs
compile ((Skip):xs) = compile xs

lexer :: String -> [Token]
lexer [] = []
lexer str
    | isPrefixOf " " str = lexer (dropWhile (== ' ') str)
    | otherwise = case find (`isPrefixOf` str) delimiters of
        Just delimiter -> stringToToken delimiter : lexer (drop (length delimiter) str)
        Nothing -> let (token, rest) = break (`elem` operatorChars) str
                   in if not (null token) then stringToToken token : lexer rest else lexer rest
  where
    delimiters = ["+","-","*","<=","==",":=", "=","<","(",")","{","}",";","not","and","or","if","then","else","while","do","True","False", "i", "fact", "do"]
    operatorChars = ' ' : nub (concat delimiters)

stringToToken :: String -> Token
stringToToken "+" = PlusTok
stringToToken "-" = MinusTok
stringToToken "*" = TimesTok
stringToToken "/" = DivTok
stringToToken "(" = OpenTok
stringToToken ")" = CloseTok
stringToToken ":=" = AssignTok
stringToToken "while" = WhileTok
stringToToken "do" = DoTok
stringToToken "True" = TrueTok
stringToToken "False" = FalseTok
stringToToken "and" = AndTok
stringToToken "or" = OrTok
stringToToken "not" = NotTok
stringToToken "==" = EqTok
stringToToken "<=" = LeTok
stringToToken "if" = IfTok
stringToToken "then" = ThenTok
stringToToken "else" = ElseTok
stringToToken ";" = SemicolonTok
stringToToken str | all isDigit str = IntTok (read str)
stringToToken str = VarTok str

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

parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens = parseOr tokens

parseOr :: [Token] -> Maybe (Bexp, [Token])
parseOr tokens = do
  (bexp1, restTokens1) <- parseAnd tokens
  parseOr' bexp1 restTokens1

parseOr' :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseOr' bexp1 (OrTok : restTokens) = do
  (bexp2, restTokens2) <- parseAnd restTokens
  parseOr' (bexp1 :|: bexp2) restTokens2
parseOr' bexp1 tokens = Just (bexp1, tokens)

parseAnd :: [Token] -> Maybe (Bexp, [Token])
parseAnd tokens = do
  (bexp1, restTokens1) <- parseNot tokens
  parseAnd' bexp1 restTokens1

parseAnd' :: Bexp -> [Token] -> Maybe (Bexp, [Token])
parseAnd' bexp1 (AndTok : restTokens) = do
  (bexp2, restTokens2) <- parseNot restTokens
  parseAnd' (bexp1 :&: bexp2) restTokens2
parseAnd' bexp1 tokens = Just (bexp1, tokens)

parseNot :: [Token] -> Maybe (Bexp, [Token])
parseNot (NotTok : restTokens) = do
  (bexp, restTokens2) <- parseBexpFactor restTokens
  Just (Not bexp, restTokens2)
parseNot tokens = parseBexpFactor tokens

parseRel :: [Token] -> Maybe (Bexp, [Token])
parseRel tokens = do
  (aexp1, restTokens1) <- parseAexp tokens
  case restTokens1 of
    (LeTok : restTokens2) -> do
      (aexp2, restTokens3) <- parseAexp restTokens2
      Just (aexp1 :<=: aexp2, restTokens3)
    (EqTok : restTokens2) -> do
      (aexp2, restTokens3) <- parseAexp restTokens2
      Just (aexp1 :==: aexp2, restTokens3)
    _ -> Nothing

parseBexpFactor :: [Token] -> Maybe (Bexp, [Token])
parseBexpFactor (OpenTok : restTokens) = do
  (bexp, CloseTok : restTokens2) <- parseBexp restTokens
  Just (bexp, restTokens2)
parseBexpFactor (TrueTok : restTokens) = Just (BoolConst True, restTokens)
parseBexpFactor (FalseTok : restTokens) = Just (BoolConst False, restTokens)
parseBexpFactor tokens =
  case parseRel tokens of
    Just (bexp, restTokens) -> Just (bexp, restTokens)
    Nothing ->
      case parseAexpOrParent tokens of
        Just (aexp, restTokens) -> Just (aexp :==: Num 0, restTokens) -- Handle the case when only an arithmetic expression is present
        _ -> Nothing


parseElse :: [Token] -> Maybe ([Stm], [Token])
parseElse tokens = 
    case tokens of
        (OpenTok : restTokens) -> loopOpen restTokens []
        _ -> loopSemicolon tokens []
  where
    loopOpen [] acc = Nothing
    loopOpen (SemicolonTok : CloseTok : SemicolonTok : restTokens) acc = Just (reverse acc, restTokens)
    loopOpen tokens acc =
      case parseStm tokens of
        Just (stm,  restTokens) -> loopOpen restTokens (stm : acc)
        _ -> Nothing

    loopSemicolon [] acc = Nothing
    loopSemicolon (SemicolonTok : restTokens) acc = Just (reverse acc, restTokens)
    loopSemicolon tokens acc =
      case parseStm tokens of
        Just (stm,  restTokens) -> loopSemicolon restTokens (stm : acc)
        _ -> Nothing

parseSeqUntilDo :: [Token] -> Maybe ([Bexp], [Token])
parseSeqUntilDo tokens = loop tokens []
  where
    loop [] acc = Nothing
    loop (DoTok : rest) acc = Just (reverse acc, DoTok : rest)
    loop tokens acc =
      case parseBexpFactor tokens of
        Just (stm, restTokens) ->
          case restTokens of
            (SemicolonTok : restTokens') -> loop restTokens' (stm : acc)
            _ -> Just (reverse (stm : acc), restTokens)
        Nothing -> Nothing

parseStm :: [Token] -> Maybe (Stm, [Token])
parseStm tokens = do
  (stm, restTokens) <- parseStm' tokens
  case restTokens of
    (SemicolonTok : restTokens2) -> do
      (stmSeq, restTokens3) <- parseStmSeq restTokens2
      Just (Seq (stm : stmSeq), restTokens3)
    _ -> Just (stm, restTokens)

parseStm' :: [Token] -> Maybe (Stm, [Token])
parseStm' tokens = 
  case tokens of
    (VarTok v : AssignTok : restofTokens) ->
      case parseAddOrSubMultOrAexpOrParent restofTokens of
        Just (expr, restTokens2) -> Just (v :=: expr, restTokens2)
        Nothing -> Nothing
    (IfTok : restofTokens) ->
      case parseBexp restofTokens of
        Just (bexp, ThenTok : restTokens2) ->
          case parseStmSeq restTokens2 of
            Just (stm1, ElseTok : restTokens3) ->
              case parseStmSeq restTokens3 of
                Just (stm2, restTokens4) -> Just (If [bexp] stm1 stm2, restTokens4)
                Nothing ->
                  case parseStm restTokens3 of
                    Just (stmElse, restTokens4) -> Just(If [bexp] stm1 [stmElse], restTokens4)
                    Nothing -> error "Error parsing else"
            Nothing -> error "Error parsing then"
        Nothing -> error "Error parsing if condition"
    (WhileTok : restofTokens) ->
      case parseBexp restofTokens of
        Just (bexp, DoTok : restTokens2) ->
          case parseStmSeq restTokens2 of
            Just (stm, restTokens3) -> Just (While [bexp] stm, restTokens3)
            Nothing -> Nothing
        Nothing -> Nothing
    (OpenTok : VarTok v : AssignTok : restofTokens) ->
      case parseAddOrSubMultOrAexpOrParent restofTokens of
        Just (expr, restTokens2) -> Just (v :=: expr, restTokens2)
        Nothing -> Nothing
    (SemicolonTok : restTokens) -> 
        Just (Skip, restTokens)
    (CloseTok : restTokens) ->
        Just (Skip, restTokens)
    _ -> Nothing

parseStmSeq :: [Token] -> Maybe ([Stm], [Token])
parseStmSeq tokens = 
  case parseStm' tokens of
    Just (stm, restTokens) ->
      case restTokens of
        (SemicolonTok : restTokens2) ->
          case parseStmSeq restTokens2 of
            Just (stmSeq, restTokens3) -> Just (stm : stmSeq, restTokens3)
            Nothing -> Just ([stm], restTokens2)
        _ -> Just ([stm], restTokens)
    Nothing -> Nothing

parse :: String -> [Stm]
parse str = parseProgram (lexer str)

parseProgram :: [Token] -> [Stm]
parseProgram tokens = 
  case tokens of
    [] -> []
    _ ->
      case parseStm tokens of
        Just (stm, restTokens) -> stm : parseProgram restTokens
        _ -> []

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyState)

main :: IO ()
main = do
    -- let tokens = lexer "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)"
    -- print tokens
    -- print $ parseStm tokens
    print $ testParser "x := 5; x := x - 1;" == ("","x=4")
    print $ testParser "if (not True and 2 <= 5 and 3 == 4) then x :=1; else y := 2;" == ("","y=2")
    print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
    print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
    print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
    print $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
    print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

