import Compiler
{-
data Token = Token {
  tokenType :: TokenType,
  tokenValue :: TokenValue,
  tokenPosition :: Position
}

data Expr =
    OpAdd Expr Expr
    | OpSub Expr Expr
    | OpMult Expr Expr
    | Num Integer
    deriving (Eq, Show)

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
-}


parseIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseIntOrParenExpr ((Token TOK_INT (TokenInt n) _ ) : restTokens) =
    Just (Num $ fromIntegral n, restTokens)
parseIntOrParenExpr ((Token TOK_SPECIAL TokenOpenParenthesis _): restTokens1) =
    case parseSumOrSubOrProdOrIntOrPar restTokens1 of
        Just (expr, (Token TOK_SPECIAL TokenClosedParenthesis _): restTokens2) ->
            Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
        Nothing -> Nothing
parseIntOrParenExpr tokens = Nothing

parseIntOrParenOrMult :: [Token] -> Maybe (Expr, [Token])
parseIntOrParenOrMult tokens =
    case parseIntOrParenExpr tokens of
        Just (expr1, (Token TOK_OPERATOR TokenMult _) : restTokens1) ->
            case parseIntOrParenOrMult restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (OpMult expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result -- can be ’Nothing’ or valid

parseSumOrSubOrProdOrIntOrPar:: [Token] -> Maybe (Expr, [Token])
parseSumOrSubOrProdOrIntOrPar tokens =
    case parseIntOrParenOrMult tokens of
        Just (expr1,(Token TOK_OPERATOR TokenPlus _) : restTokens1) ->
            case parseSumOrSubOrProdOrIntOrPar restTokens1 of
                Just (expr2,restTokens2) ->
                    Just (OpAdd expr1 expr1,restTokens2)
                Nothing -> Nothing
        Just (expr1,(Token TOK_OPERATOR TokenSub _) : restTokens1) ->
            case parseSumOrSubOrProdOrIntOrPar restTokens1 of
                Just (expr2,restTokens2) ->
                    Just (OpAdd expr1 expr1,restTokens2)
                Nothing -> Nothing
        result -> result
