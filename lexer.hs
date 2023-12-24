--  This file implements a lexer for haskell
import Data.List
import Data.Char
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
  line :: Integer,
  column :: Integer
} deriving (Show)

data TokenValue =
  -- Special Token Values
   TokenOpenParenthesis
  | TokenClosedParenthesis
  | TokenEndOfStatement

  -- Literal Values
  | TokenInt Integer
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
  | TokenEq
  | TokenNe
  | TokenLe
  | TokenIntEq
  |TokenBoolEq
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
} deriving (Show)


validOperators :: [String]
validOperators = ["+", "-", "*", "=", ":", "!"]

validKeywords :: [String]
validKeywords = ["while","do","if","then","else"]

validBool :: [String]
validBool = ["True","False"]

validSpecialChars :: [String]
validSpecialChars = ["(",")",";"]

stringToInt :: String -> Integer
stringToInt str = foldl (\acc c -> acc * 10 + charToInt c) 0 str

charToInt :: Char -> Integer
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
 | c =='9' = 9
 | otherwise = error "Unexpected digit"

-- Make is valid Identifier 
lexerOperator :: String -> TokenValue
lexerOperator op =
  case op of 
    "+" -> TokenPlus
    "-" -> TokenSub
    "*" -> TokenMult
    "&&" -> TokenAnd
    ":= " -> TokenAssign
    "!=" -> TokenLe
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

lexerSpecial special =
  case special of 
    ";" -> TokenEndOfStatement
    "(" -> TokenOpenParenthesis
    ")" -> TokenClosedParenthesis
    _ -> error $ "Lexer Error: Keyword {" ++ special ++"} does not exist"

lexerBool bool =
  case bool of
    "True" -> TokenBool True
    "False" -> TokenBool False
    _ -> error $ "Lexer Error: Keyword {" ++ bool ++"} does not exist"

lexer :: String -> [Token]
lexer = lexer_aux 1 1 

lexer_aux :: Integer -> Integer -> String -> [Token]
lexer_aux line column [] = []
lexer_aux line column (x:xs)
  -- Handle Numbers
  | isDigit x =
    let (numeric, rest) = span isDigit (x:xs)
        newPosition = Position line column
        newType = TOK_INT
        newValue = TokenInt (stringToInt numeric)
      in Token newType newValue newPosition : lexer_aux line (column + fromIntegral (length numeric)) rest
   -- Handle Identifiers and Keywords
   | isAlpha x =
    let (ident, rest) = span isAlpha (x:xs)
        newPosition = Position line column
        newType  
          | ident `elem` validKeywords = TOK_KEYWORD 
          | ident `elem` validBool = TOK_BOOL 
          | otherwise = TOK_IDENT
        newValue 
          | ident `elem` validKeywords = lexerKeyword ident
          | ident `elem` validBool = lexerBool ident
          | otherwise = TokenIdent ident
      in Token newType newValue newPosition : lexer_aux line (column + fromIntegral (length ident)) rest

  -- Handle Operators
  | x `elem` validOperators =
          let (operator, rest) = span validOperators (x:xs)
              newPosition = Position line column
              newType = TOK_OPERATOR
              newValue = lexerOperator operator
          in Token newType newValue newPosition : lexer_aux line (column + fromIntegral (length operator)) rest
  -- Handle Special chars
  | x `elem` validSpecialChars =
        let (operator, rest) = span validOperators (x:xs)
            newPosition = Position line column
            newType = TOK_SPECIAL
            newValue = lexerSpecial x
          in Token newType newValue newPosition : lexer_aux line (column + 1) xs
  | isSpace x = 
      lexer_aux line (column + 1) xs
  -- Error
  | otherwise = 
      error $ "Lexer error. Token {" ++ x ++ "} on {" ++ line ++ " " ++ column ++ "} not recognized"

