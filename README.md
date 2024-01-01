#  Turma 5 Grupo 8 - 2nd Project PFL

  

##  Index

- [Group](#group-and-contributions)

- [Lexer](#lexer)




##  Group and Contributions

- Tomás Alexandre Torres Pereira (up202108845@edu.fe.up.pt), contribution -> 

- Tomás Miranda de Figueiredo Sarmento (up202108778@edu.fe.up.pt), contribution -> 

  

##  Lexer
A lexer, or lexical analyzer, is the initial stage in language compilation. This lexer searches for patterns and deconstructs an input string into tokens. These tokens are then utilized for identifiers, operators, special characters, or keywords.
### Tokens
Tokens are defined within a Haskell data block in our lexer. Each token is characterized by a type, such as "identifier" or "operator," and is associated with a corresponding value and position. The position field plays a crucial role during debugging, enabling us to pinpoint the line and column where any issues with the input occur. This feature aids in generating informative error messages, helping identify the location of problems and facilitating the debugging process

```haskell
data Token = Token {
  tokenType :: TokenType,
  tokenValue :: TokenValue,
  tokenPosition :: Position
} 
```
Token types: 
	
	TOK_SPECIAL,TOK_INT,TOK_BOOL,TOK_IDENT,TOK_KEYWORD,TOK_OPERATOR

```
```
> **_NOTE:_**    We included token types in our lexer because lexers typically utilize them. Without this specification, the structure would technically resemble that of a tokenizer.
```
```
  
Token values: 

	TokenClosedParenthesis,TokenEndOfStatement,TokenInt Int,TokenBool Bool,TokenIdent String,TokenWhile,TokenDo,TokenIf,TokenThen,TokenElse,TokenPlus,TokenSub,TokenMult,TokenIntEq,TokenBoolEq,TokenLe,TokenNot,TokenAnd,TokenAssign
  

Token Position: We define our token position within a Haskell data block, which includes both the line and column of the token recovered from the input string by the lexer.
```haskell
data Position = Position {
  line :: Int,
  column :: Int
} deriving (Show)
```
```
```
> **_NOTE:_**    We incorporated token position in our lexer to enhance the user experience during debugging.
```
```
The main lexer function is defined by 
```haskell
  lexer:: String -> [Tokens]
   ```
   This function will call a helper lexer_aux that does the main work of the lexer. This happens because in lexer we need to call ( lexer_aux 1 1 string)  to get the position of tokens.

Our lexer primarily operates through pattern-matching strategies. It begins by examining the first character and selects various pattern-matching branches based on the character type encountered by the lexer. These branches cover character types such as digits, alphabets, special characters, operators, or spaces. If the lexer does not find a match in any of these branches, it issues an error message indicating that the character is not accepted by the lexer.

Digits ->
```haskell
 | isDigit x =
    let (numeric, rest) = my_span isDigit (x:xs)
        newPosition = Position line column
        newType = TOK_INT
        newValue = TokenInt (stringToInt numeric)
    in Token newType newValue newPosition : lexer_aux line (column + length numeric) rest

```
In this case the type is TOK_INT and the value is calculated by a helper function defined stringToInt that converts a numeric string to a real integer value.   

Alphabet ->
```haskell
  | isAlpha x =
    let (ident, rest) = my_span isAlphaNum (x:xs)
        newPosition = Position line column
        newType = case () of
          _ | ident `my_elem` validKeywords -> TOK_KEYWORD
            | ident `my_elem` validBool -> TOK_BOOL
            | ident `my_elem` validOperators -> TOK_OPERATOR
            | otherwise -> TOK_IDENT
        newValue = case () of
          _ | ident `my_elem` validKeywords -> lexerKeyword ident
            | ident `my_elem` validBool -> lexerBool ident
            | ident `my_elem` validOperators -> lexerOperator ident
            | otherwise -> TokenIdent ident
    in Token newType newValue newPosition : lexer_aux line (column + length ident) rest

```
When the character belongs to the alphabet, haskell will call my span for x:xs and retrieve a string until the char is different from space.  Wit the new ident , the type can be calculated  looking in the arrays defined for different valid types.  The value needs to be a TokenValue, and for that helper functions like lexerBool or lexerKeyword will associate different string values to respective token values.

All keywords, special chars and operators follow the same logic as the isDigit branch, but with their respective token values and token types,

Spaces ->
```haskell
| isSpace x =
    lexer_aux line (column + 1) xs
```
When this happens, lexer skips a character and increases column value.

**Functions to convert strings to token values**

```haskell
lexerOperator :: String -> TokenValue
lexerOperator op =
  case op of 
    "+" -> TokenPlus
    "-" -> TokenSub
    "*" -> TokenMult
    "and" -> TokenAnd
    ":=" -> TokenAssign
    "not" -> TokenNot
    "<=" -> TokenLe
    "==" -> TokenIntEq
    "=" -> TokenBoolEq
    _  -> error $ "Lexer Error: Operator {" ++ op ++ "} does not exist"

-- Converts a keyword string to it's corresponding token value.
lexerKeyword :: String -> TokenValue
lexerKeyword keyword =
  case keyword of 
    "while" -> TokenWhile
    "do" -> TokenDo
    "if" -> TokenIf
    "else" -> TokenElse
    "then" -> TokenThen
    _  -> error $ "Lexer Error: Keyword {" ++ keyword ++ "} does not exist"

-- Converts a special character string to it's corresponding token value.
lexerSpecial :: String -> TokenValue
lexerSpecial special =
  case special of 
    ";" -> TokenEndOfStatement
    "(" -> TokenOpenParenthesis
    ")" -> TokenClosedParenthesis
    _ -> error $ "Lexer Error: Keyword {" ++ special ++"} does not exist"

-- Converts a boolean string to it's corresponding token value.
lexerBool :: String -> TokenValue
lexerBool bool =
  case bool of
    "True" -> TokenBool True
    "False" -> TokenBool False
    _ -> error $ "Lexer Error: Keyword {" ++ bool ++"} does not exist"
    
```

**Print tokens**
For debgging purposes we defined a new show function for a Token for better user experience.
```haskell
instance Show Token where
  show (Token token_type token_value token_position) =
    " \nToken:\n"
    ++ "  Type: " ++ show token_type ++ "\n"
    ++ "  Value: " ++ show token_value ++ "\n"
    ++ "  Position: " ++ show token_position ++ "\n"
```

```
```
> **_NOTE:_**     The line in position will never change for this assignment.
```
```
