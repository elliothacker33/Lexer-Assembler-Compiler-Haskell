--  This file implements a lexer for haskell
module Lexer where 

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

-- All tokens that our lexer recognize as valid Tokens
data TokenType =
  -- Special Tokens
  TOK_SPECIAL

  -- Literals
  | TOK_INT
  | TOK_BOOL

  -- Identifiers and Keywords
  | TOK_IDENT
  | TOK_KEYWORD

  -- Operators
  | TOK_OPERATOR

  deriving (Show)

data Position = Position {
  line :: Int,
  column :: Int
} deriving (Show)

data TokenValue =
  -- Special Token Values
  TokenOpenParenthesis
  | TokenClosedParenthesis
  | TokenEndOfStatement

  -- Literal Values
  | TokenInt Int
  | TokenBool Bool

  -- Identifier and Keyword Values
  | TokenIdent String
  | TokenWhile
  | TokenDo
  | TokenIf
  | TokenThen
  | TokenElse

  -- Arithmetic Operators
  | TokenPlus
  | TokenSub
  | TokenMult

  -- Comparison Operators
  | TokenIntEq
  | TokenBoolEq
  | TokenLe

  -- Logical Operators
  | TokenNot
  | TokenAnd

  -- Assignment Operators
  | TokenAssign

  deriving (Show)

data Token = Token {
  tokenType :: TokenType,
  tokenValue :: TokenValue,
  tokenPosition :: Position
} 

instance Show Token where
  show (Token token_type token_value token_position) =
    " \nToken:\n"
    ++ "  Type: " ++ show token_type ++ "\n"
    ++ "  Value: " ++ show token_value ++ "\n"
    ++ "  Position: " ++ show token_position ++ "\n"

validOperators :: [String]
validOperators = ["+", "-", "*", "=", ":", "!"]

validKeywords :: [String]
validKeywords = ["while","do","if","then","else"]

validBool :: [String]
validBool = ["True","False"]

validSpecialChars :: [String]
validSpecialChars = ["(",")",";"]

my_elem :: String -> [String] -> Bool
my_elem el [] = False
my_elem el (x:xs)
  | (x == el) = True
  | otherwise = my_elem el xs

my_span :: (Char -> Bool) -> String -> (String, String)
my_span _ [] = ([], [])
my_span f (x:xs)
  | f x = let (matched, rest) = my_span f xs in (x : matched, rest)
  | otherwise = ([], x:xs)

stringToInt :: String -> Int
stringToInt str = case str of
  ('-' : rest) -> -1 * foldl (\acc c -> acc * 10 + charToInt c) 0 rest
  _ -> foldl (\acc c -> acc * 10 + charToInt c) 0 str

charToInt :: Char -> Int
charToInt c
  | c == '0' = 0
  | c == '1' = 1
  | c == '2' = 2
  | c == '3' = 3
  | c == '4' = 4
  | c == '5' = 5
  | c == '6' = 6
  | c == '7' = 7
  | c == '8' = 8
  | c == '9' = 9
  | otherwise = error $ "Unexpected digit"

-- Make is valid Identifier 
lexerOperator :: String -> TokenValue
lexerOperator op =
  case op of 
    "+" -> TokenPlus
    "-" -> TokenSub
    "*" -> TokenMult
    "&&" -> TokenAnd
    ":=" -> TokenAssign
    "!" -> TokenNot
    "<=" -> TokenLe
    "==" -> TokenIntEq
    "=" -> TokenBoolEq
    _  -> error $ "Lexer Error: Operator {" ++ op ++ "} does not exist"

lexerKeyword :: String -> TokenValue
lexerKeyword keyword =
  case keyword of 
    "while" -> TokenWhile
    "do" -> TokenDo
    "if" -> TokenIf
    "else" -> TokenElse
    "then" -> TokenThen
    _  -> error $ "Lexer Error: Keyword {" ++ keyword ++ "} does not exist"

lexerSpecial :: String -> TokenValue
lexerSpecial special =
  case special of 
    ";" -> TokenEndOfStatement
    "(" -> TokenOpenParenthesis
    ")" -> TokenClosedParenthesis
    _ -> error $ "Lexer Error: Keyword {" ++ special ++"} does not exist"

lexerBool :: String -> TokenValue
lexerBool bool =
  case bool of
    "True" -> TokenBool True
    "False" -> TokenBool False
    _ -> error $ "Lexer Error: Keyword {" ++ bool ++"} does not exist"

lexer :: String -> [Token]
lexer = lexer_aux 1 1 

lexer_aux :: Int -> Int -> String -> [Token]
lexer_aux line column [] = []
lexer_aux line column (x:xs)
  | isDigit x =
    let (numeric, rest) = my_span isDigit (x:xs)
        newPosition = Position line column
        newType = TOK_INT
        newValue = TokenInt (stringToInt numeric)
    in Token newType newValue newPosition : lexer_aux line (column + length numeric) rest

  | isAlpha x =
    let (ident, rest) = my_span isAlphaNum (x:xs)
        newPosition = Position line column
        newType = case () of
          _ | ident `my_elem` validKeywords -> TOK_KEYWORD
            | ident `my_elem` validBool -> TOK_BOOL
            | otherwise -> TOK_IDENT
        newValue = case () of
          _ | ident `my_elem` validKeywords -> lexerKeyword ident
            | ident `my_elem` validBool -> lexerBool ident
            | otherwise -> TokenIdent ident
    in Token newType newValue newPosition : lexer_aux line (column + length ident) rest

  | [x] `my_elem` validOperators =
    let (operator, rest) = my_span (\c -> [c] `my_elem` validOperators) (x:xs)
        newPosition = Position line column
        newType = TOK_OPERATOR
        newValue = lexerOperator operator
    in Token newType newValue newPosition : lexer_aux line (column + length operator) rest

  | [x] `my_elem` validSpecialChars =
    let newPosition = Position line column
        newType = TOK_SPECIAL
        newValue = lexerSpecial [x]
    in Token newType newValue newPosition : lexer_aux line (column + 1) xs

  | isSpace x =
    lexer_aux line (column + 1) xs

  | otherwise =
    error $ "Lexer error. Token {" ++ [x] ++ "} on {" ++ show line ++ " " ++ show column ++ "} not recognized"
