import Compiler
import Lexer

data Expr = 
     IntLit Int
    | BoolLit Int
    | Identifier String
    | Add Expr Expr
    | Sub Expr Expr
    | Mult Expr Expr 
    | Assign Expr Expr


parseInt :: [Token] -> Maybe (Expr, [Token])
parseInt (_ TOK_INT _ number _ : restTokens) = Just (IntLit n, restTokens)
parseInt tokens = Nothing

parseBool :: [Token] -> Maybe (Expr,[Token])
parseBool (_ TOK_BOOL _ number _ : restTokens) = Just (BoolLit n, restTokens)
parseBool tokens = Nothing

parseIdent :: [Token] -> Maybe (Expr,[Token])
parseIdent (_ TOK_IDENT _ var _ : restTokens) = Just (Identifier n, restTokens)
parseIdent tokens = Nothing

parseAssign :: [Token] -> Maybe (Expr, [Token])
parseAssign tokens =
    case parseIdent tokens of 
        Just (expr1,(TokenAssign : restTokens1)) -> case parseExpr restTokens1 of
            Just(expr2,restTokens2) -> Just (Assign expr1 expr2)
            Nothing -> Nothing
        result -> result

parseSum :: [Token] -> Maybe (Expr, [Token])
parseSum tokens =
  case parseInt tokens of
    Just (expr1, (TokenPlus : restTokens1)) -> case parseExpr restTokens1 of
        Just (expr2, restTokens2) -> Just (Add expr1 expr2, restTokens2)
        Nothing -> Nothing
    result -> result

parseSub :: [Token] -> Maybe (Expr, [Token])
parseSub tokens = 
    case parseInt tokens of
        Just (expr1,(TokenSub : restTokens1)) -> case parseExpr restTokens1 of
            Just(expr2,restTokens2) -> Just (Sub expr1 expr2, restTokens2)
    result->result 

parseMult :: [Token] -> Maybe (Expr,[Token])
parseMult tokens = 
    case parseInt tokens of
        Just (expr1, (TokenMult : restTokens1)) -> case parseExpr restTokens1 of
            Just (expr2,restTokens2) -> Just (Mult expr1 expr2, restTokens2)
            Nothing -> Nothing
        result -> result



parseExpr :: [Token] -> Maybe (Expr, [Token])
parseExpr tokens =
  parseMult tokens <|> parseSum tokens <|> parseSub tokens <|> parseParen tokens <|> parseInt tokens

parse :: [Token] -> Expr
parse tokens =
  case parseExpr tokens of
    Just (expr, []) -> expr
    _ -> error "Failed to parse expression"

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