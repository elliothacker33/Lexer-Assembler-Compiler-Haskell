#  Turma 5 Grupo 8 - 2nd Project PFL

  

##  Index

- [Group](#group-and-contributions)

- [Lexer](#lexer)

- [Parser](#parser)




##  Group and Contributions

- Tomás Alexandre Torres Pereira (up202108845@edu.fe.up.pt), contribution -> 

- Tomás Miranda de Figueiredo Sarmento (up202108778@edu.fe.up.pt), contribution -> 

## Parser
- this functions calls the lexer to transform the string in tokens and the it calls the 
```haskell
  parse :: String -> [Stm]
  parse s = parseTmp $ lexer s
```
- parseTmp runs parseT in the tokens until there is none tokens left to parse because there can be a lot of tokens sequencial and the function parseT only parses a statement at a time like a if or a while
```haskell
parseTmp :: [Token] -> [Stm]
parseTmp tokens = 
    case parseT tokens of
        Just (statements, []) ->
            statements
        Just (statements,s) ->
            statements ++ parseTmp s
```
- parseT
```haskell
parseT :: [Token] -> Maybe ([Stm], [Token])
```

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

> **_NOTE:_**    We included token types in our lexer because lexers typically utilize them. Without this specification, the structure would technically resemble that of a tokenizer.
  
Token values: 

	TokenClosedParenthesis,TokenEndOfStatement,TokenInt Int,TokenBool Bool,TokenIdent String,TokenWhile,TokenDo,TokenIf,TokenThen,TokenElse,TokenPlus,TokenSub,TokenMult,TokenIntEq,TokenBoolEq,TokenLe,TokenNot,TokenAnd,TokenAssign
  

Token Position: We define our token position within a Haskell data block, which includes both the line and column of the token recovered from the input string by the lexer.
```haskell
data Position = Position {
  line :: Int,
  column :: Int
} deriving (Show)
```

> **_NOTE:_**    We incorporated token position in our lexer to enhance the user experience during debugging.

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


> **_NOTE:_**     The line in position will never change for this assignment.
```
```

## parseT
- the struct used for the parse are for the Stm
```haskell
data Stm =
    IfThenElse Bexp [Stm] [Stm] -- condiction  then code else code2
    | StmLoop Bexp [Stm] -- conmdiction code
    | NewVar String Aexp -- name of the variable      -- exprestion aritemetic
    deriving (Show)
```
- the struct used for aritemetic expresions 
```haskell
data Aexp = 
    OpAdd Aexp Aexp
    | OpMult Aexp Aexp
    | OpSub Aexp Aexp
    | Num Integer
    | GetVar String
    deriving (Eq, Show)
```
- the struct used for the boolean expretions
```haskell
data Bexp =
    Bo Bool
    | IntEqual Aexp Aexp
    | Equal Bexp Bexp
    | AndOp Bexp Bexp
    | LessOrEqual Aexp Aexp
    | Negation Bexp
    deriving (Eq, Show)
```
- This function given a list of tokens it parses the first statement it founds like a if , a while or a 'x:=3 ' and returns the rest of the tokens to be parsed further.The parseT function parses all statements related to the first statement it finds like in a if it might be a "x:=3" or three inside this function parses it
### parse aritemetic operations
- to parse the aritemetic functions with the priorities of the operations we defined the function **parseSumOrSubOrProdOrIntOrPar** with help of the teacher at the beginning of this function we call the **parseIntOrParenOrMult** because it has a higher priority and in the begining of this function we called **parseIntOrParenExpr** this function has the highest priotity because a int cannot be broken further and a parentesis can change the priority of the operations wich makes sense be with the highest priority

```
parseIntOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenExpr ((Token TOK_INT (TokenInt n) _ ) : restTokens) =
    Just (Num $ fromIntegral n, restTokens)
parseIntOrParenExpr ((Token TOK_IDENT (TokenIdent name) _ ) : restTokens) =
    Just (GetVar name, restTokens)
parseIntOrParenExpr ((Token TOK_SPECIAL TokenOpenParenthesis _): restTokens1) =
    case parseSumOrSubOrProdOrIntOrPar restTokens1 of
        Just (expr, (Token TOK_SPECIAL TokenClosedParenthesis _): restTokens2) ->
            Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
        Nothing -> Nothing
parseIntOrParenExpr tokens = Nothing

parseIntOrParenOrMult :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenOrMult tokens =
    case parseIntOrParenExpr tokens of
        Just (expr1, (Token TOK_OPERATOR TokenMult _) : restTokens1) ->
            case parseIntOrParenOrMult restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (OpMult expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result -- can be ’Nothing’ or valid

parseSumOrSubOrProdOrIntOrPar:: [Token] -> Maybe (Aexp, [Token])
parseSumOrSubOrProdOrIntOrPar tokens =
    case parseIntOrParenOrMult tokens of
        Just (expr1,(Token TOK_OPERATOR TokenPlus _) : restTokens1) ->
            case parseSumOrSubOrProdOrIntOrPar restTokens1 of
                Just (expr2,restTokens2) ->
                    Just (OpAdd expr1 expr2,restTokens2)
                Nothing -> Nothing
        Just (expr1,(Token TOK_OPERATOR TokenSub _) : restTokens1) ->
            case parseSumOrSubOrProdOrIntOrPar restTokens1 of
                Just (expr2,restTokens2) ->
                    Just (OpSub expr1 expr2,restTokens2)
                Nothing -> Nothing
        result -> result

```

- To parse the bolleans operations we implemented a similar algorithm 
- Firstly we implemented **parseBoolOrParenOrEqualOrLeOrNotOrAndExpr** is responsible to parse the and operation between bolleans but because it the operation between bolleans with least priority in the begining we call **parseBoolOrParenOrEqualOrLeOrNotExpr** wich is responsible for the operation equality between bolleans because there are operations with more priority we called in the beginning **parseNotOrBoolOrParenOrIntcompExpr**

```haskell
parseBoolOrParenOrEqualOrLeOrNotExpr:: [Token] -> Maybe (Bexp, [Token])
parseBoolOrParenOrEqualOrLeOrNotExpr tokens = 
    case parseNotOrBoolOrParenOrIntcompExpr tokens of
        Just (expr1,(Token TOK_OPERATOR TokenBoolEq _) : restTokens1) ->
            case parseBoolOrParenOrEqualOrLeOrNotExpr restTokens1 of
                Just (expr2,restTokens2) ->
                    Just (Equal expr1 expr2,restTokens2)
                Nothing -> Nothing
        result -> result


parseBoolOrParenOrEqualOrLeOrNotOrAndExpr:: [Token] -> Maybe (Bexp, [Token])
-- operations with booleans
parseBoolOrParenOrEqualOrLeOrNotOrAndExpr tokens = 
    case parseBoolOrParenOrEqualOrLeOrNotExpr tokens of
        Just (expr1,(Token TOK_OPERATOR TokenAnd _) : restTokens1) ->
            case parseBoolOrParenOrEqualOrLeOrNotOrAndExpr restTokens1 of
                Just (expr2,restTokens2) ->
                    Just (AndOp expr1 expr2,restTokens2)
                Nothing -> Nothing
        result -> result

```
- **parseNotOrBoolOrParenOrIntcompExpr**
- this function is responsible for parsing comparations between integers not parentesis and booleans
- this is the lowest degree of the parse of the booleans because of it in the begining of this function we dont call any function is particular
- to parse integer comparison we use the parser
- this function is used to parse integer expr in bolleans because both aritemetic expretions and bollean expretions both use parentesis if the parse fails it tries to use the parentesis on a bollean basis
```haskell
auxForAritemeticBolleanExpr :: [Token] -> Maybe (Bexp, [Token])
auxForAritemeticBolleanExpr tokens = 
    case parseSumOrSubOrProdOrIntOrPar tokens of
        Just (expr1,(Token TOK_OPERATOR TokenLe _) : restTokens1) ->
            case parseSumOrSubOrProdOrIntOrPar restTokens1 of
                Just (expr2,restTokens2) ->
                    Just (LessOrEqual expr1 expr2,restTokens2)
                Nothing -> Nothing
        Just (expr1,(Token TOK_OPERATOR TokenIntEq _) : restTokens1) ->
            case parseSumOrSubOrProdOrIntOrPar restTokens1 of
                Just (expr2,restTokens2) ->
                    Just (IntEqual expr1 expr2,restTokens2)
                Nothing -> Nothing
        a-> bolleanParentesis tokens
```
- the function that handles bollean parenthesis is **bolleanParentesis** this function is called after the **auxForAritemeticBolleanExpr** to make sure this parenthesis are to give priority to bolleans and not to aritemetic expresitions

```haskell

bolleanParentesis :: [Token] -> Maybe (Bexp,[Token])
bolleanParentesis ((Token TOK_SPECIAL TokenOpenParenthesis p): restTokens1) =
    case parseBoolOrParenOrEqualOrLeOrNotOrAndExpr restTokens1 of
        Just (expr, (Token TOK_SPECIAL TokenClosedParenthesis _): restTokens2) ->
            Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
bolleanParentesis a = error $ "didn't expect " ++ show (head a)
```
- to parse not and the bollean it self as well as the functions that are above they are called like this 
```haskell
parseNotOrBoolOrParenOrIntcompExpr :: [Token] -> Maybe (Bexp, [Token])
parseNotOrBoolOrParenOrIntcompExpr ((Token TOK_BOOL (TokenBool b) _ ) : restTokens) =
    Just (Bo b, restTokens)

parseNotOrBoolOrParenOrIntcompExpr ((Token TOK_SPECIAL TokenOpenParenthesis p): restTokens1) =
    auxForAritemeticBolleanExpr ((Token TOK_SPECIAL TokenOpenParenthesis p): restTokens1)
    
parseNotOrBoolOrParenOrIntcompExpr ((Token _ TokenNot _): restTokens1) = 
    case parseNotOrBoolOrParenOrIntcompExpr restTokens1 of
        Just (expr,restTokens2) ->
            Just (Negation expr, restTokens2)
parseNotOrBoolOrParenOrIntcompExpr ((Token TOK_INT (TokenInt value) p ):tokens)= 
    auxForAritemeticBolleanExpr ((Token TOK_INT (TokenInt value) p ):tokens)

parseNotOrBoolOrParenOrIntcompExpr((Token TOK_IDENT (TokenIdent value) p ):tokens) =
    auxForAritemeticBolleanExpr ((Token TOK_IDENT (TokenIdent value) p ):tokens)

parseNotOrBoolOrParenOrIntcompExpr [] = error "suddenly end of boolean expression"
parseNotOrBoolOrParenOrIntcompExpr tokens = error $ "cound't match Token :" ++ show (head tokens)
```

### ParseT
- this function is used to parse all statements one at a time lets start with the struct used "x:=3" we call the **parseSumOrSubOrProdOrIntOrPar** on the rest of the tokens to make the operation and return the rest of the tokens we they find a ; with this we using the struct Stm return **NewVar value ex1**

```haskell
parseT(Token TOK_IDENT (TokenIdent value) p : Token _ TokenAssign _ :rest)=
    case parseSumOrSubOrProdOrIntOrPar rest of
        Just (ex1,(Token _ TokenEndOfStatement _ ):rest1) ->
            Just([NewVar value ex1] ,rest1)
        Just (ex1,x) ->
            error $ "Expected end of statement but found : "++ show ( head x)
        Nothing -> error $ "something went wrong when trying to parse declare variable\n" ++ show rest
```
- Parsing the if statement is the most challanging because of all the interactions with the statements and with the parenthesis
- We defined a function two functions two deal with the parenthesis because a then in a if or ends with a statemen like 'x:= 0;' or '(x:= 0;)' when the else statement ends like 'x:= 0;' or '(x:= 0;);' using parentesis its needed a second ; after the parenthesis
```haskell
parseThenStatement:: [Token] -> Maybe ([Stm],[Token])
parseThenStatement ((Token TOK_IDENT d p):tokens)=
    parseT $ (Token TOK_IDENT d p):tokens

parseThenStatement ((Token t TokenOpenParenthesis p):tokens)=
    loopUntilCloseParentesis tokens

parseElseOrDoStatement:: [Token] -> Maybe ([Stm],[Token])
parseElseOrDoStatement ((Token TOK_IDENT d p):tokens)=
    parseT $ (Token TOK_IDENT d p):tokens

parseElseOrDoStatement ((Token t TokenOpenParenthesis p):tokens)=
    case loopUntilCloseParentesis tokens of
        Just (ex1 ,(Token t TokenEndOfStatement _):rest1) ->
            Just(ex1,rest1)
        _ -> error "error expected ; after else statement"

loopUntilCloseParentesis :: [Token] -> Maybe ([Stm],[Token])
loopUntilCloseParentesis tokens=
    case parseT tokens of
        Just(ex1,Token _ TokenClosedParenthesis _ :rest)->
            Just(ex1,rest)
        Just(ex1,[]) -> error "didn't found close parentesis"
        Just(ex1,tokens1) ->
            case loopUntilCloseParentesis tokens1 of
                Just(ex2,rest2) ->
                    Just(ex1 ++ ex2 , rest2)
```
- now snipet that parses the if then else statement using the functions above to the fullest and leaving the code clean parsing each code and bollean in different stages starting by parsing the bollean **parseBoolOrParenOrEqualOrLeOrNotOrAndExpr** then removing the 'then' token we use the **parseThenStatement** function finally when we with the output of **parseThenStatement** we remove the else statement and the we execute **parseElseOrDoStatement** and output the expr and the rest of the tokens
```haskell
parseT(Token TOK_KEYWORD TokenIf p:rest)= 
    case parseBoolOrParenOrEqualOrLeOrNotOrAndExpr rest of
        Just (condition,rest1) ->
            case parseThenStatement $ tail rest1 of
                Just (code,(Token _ TokenElse _):rest2)->
                    case parseElseOrDoStatement rest2 of
                        Just (codeelse,rest3)->
                            Just ([IfThenElse condition code codeelse],rest3)

                        _ -> error "error parsing else statement\n" 
                a ->  error $ "error parsing if code1\n" ++ show a 
        a -> error $ "expected boolean function after if\n" ++ show rest
```
- to parse the while loop we used the **parseElseOrDoStatement** given the tests We where given we belive it beaves the same way as the else the function 'while .... do x:= 3;' and 'while .... do (x:= 3;);'
```
parseT(Token _ TokenWhile p :rest)=
    case parseBoolOrParenOrEqualOrLeOrNotOrAndExpr rest of
        Just (coditicion,(Token _ TokenDo _) :rest2) ->
            case parseElseOrDoStatement rest2 of
                Just (loop,rest4) ->
                    Just([StmLoop coditicion loop ],rest4) 
                Nothing -> error "expected a do before end\n"
        _ -> error "error while parsing while loop\n" 
```

## Compile

- the compile transforms the [Stm] into Code that can be ran by the part 1 of the project
- this function focus on compiling the statements and then it runs the compB or compA based on wich is compiling a condition is compiled with compB and a code is compiled with compA

```haskell
compile :: [Stm] -> Code
compile [] = []
compile (IfThenElse boleanexp c1 c2:rest)=
    compB boleanexp ++ [Branch (compile c1) (compile c2)] ++ compile rest

compile (StmLoop boleanexp c:rest)=
    [Loop (compB boleanexp) (compile c)] ++ compile rest

compile ( NewVar string exp:rest)=
    compA exp ++ [Store string] ++ compile rest
```
### compB
```haskell
compB :: Bexp -> Code
compB (Bo False) = [Fals]
compB (Bo True) = [Tru]
compB (Negation b) = compB b ++[Neg]
compB (IntEqual e1 e2)
    = compA e2 ++ compA e1 ++ [Equ]
compB (AndOp e1 e2)
    = compB e2 ++ compB e1 ++ [And]
compB (Equal e1 e2)
    = compB e2 ++ compB e1 ++ [Equ]
compB (LessOrEqual e1 e2)
    = compA e2 ++ compA e1 ++ [Le]
```
### compA
```haskell
compA :: Aexp -> Code
compA (Num n) = [Push n]
compA  (GetVar s) = [Fetch s]
compA (OpAdd e1 e2)
    = compA e2 ++ compA e1 ++ [Add]
compA (OpMult e1 e2)
    = compA e2 ++ compA e1 ++ [Mult]
compA (OpSub e1 e2)
    = compA e2 ++ compA e1 ++ [Sub]
```