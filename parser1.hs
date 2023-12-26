{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Compiler
import Lexer

data Aexp = 
    OpAdd Aexp Aexp
    | OpMult Aexp Aexp
    | OpSub Aexp Aexp
    | Num Integer
    deriving (Eq, Show)
data Bexp =
    Bo Bool
    | And Bexp Bexp
    | IntEqual Aexp Aexp
    | Equal Bexp Bexp
    | LessOrEqual Aexp Aexp
    | Negation Bexp
    deriving (Eq, Show)
-- temos que por isto prgram
findToken :: TokenValue -> Token -> Bool
findToken t (Token _ t1 _)  = t1 == t 

compile :: [Token] -> Code
compile (Token TOK_SPECIAL TokenIf p:rest)=
    [Branch (compile r1) (compile r2), Branch (Neg:compile r1)  (compile r2)]
    where
        (r1, rest1) = span (findToken TokenThen) rest
        (r2, rest2) = span (findToken TokenElse) rest1
        (r3, rest3) = span (findToken TokenElse) rest2
        

compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (OpAdd e1 e2)
    = compA e1 ++ compA e2 ++ [Add]
compA (OpMult e1 e2)
    = compA e1 ++ compA e2 ++ [Mult]
compA (OpSub e1 e2)
    = compA e1 ++ compA e2 ++ [Sub]

compB :: Bexp -> Code
compB (Bo False) = [Fals]
compB (Bo True) = [Tru]
compB (IntEqual e1 e2)
    = compA e1 ++ compA e2 ++ [Equ]
compB (Equal e1 e2)
    = compB e1 ++ compB e2 ++ [Equ]
compB (LessOrEqual e1 e2)
    = compA e1 ++ compA e2 ++ [Le]


-- bool expr
parseBoolOrParenExpr :: [Token] -> Maybe (Bexp, [Token])
parseBoolOrParenExpr ((Token TOK_BOOL (TokenBool b) _ ) : restTokens) =
    Just (Bo b, restTokens)
parseBoolOrParenExpr ((Token TOK_SPECIAL TokenOpenParenthesis _): restTokens1) =
    case parseBoolOrParenOrEqualOrLeExpr restTokens1 of
        Just (expr, (Token TOK_SPECIAL TokenClosedParenthesis _): restTokens2) ->
            Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
        Nothing -> Nothing
parseBoolOrParenExpr tokens = Nothing


parseBoolOrParenOrEqualOrLeExpr:: [Token] -> Maybe (Bexp, [Token])
-- operations that generate bolleans
parseBoolOrParenOrEqualOrLeExpr tokens =
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
-- operations with booleans
parseBoolOrParenOrEqualOrLeExpr tokens =
    case parseBoolOrParenOrEqualOrLeExpr tokens of
        Just (expr1,(Token TOK_OPERATOR TokenBoolEq _) : restTokens1) ->
            case parseBoolOrParenOrEqualOrLeExpr restTokens1 of
                Just (expr2,restTokens2) ->
                    Just (Equal expr1 expr2,restTokens2)
                Nothing -> Nothing
        Just (expr1,(Token TOK_OPERATOR TokenAnd _) : restTokens1) ->
            case parseBoolOrParenOrEqualOrLeExpr restTokens1 of
                Just (expr2,restTokens2) ->
                    Just (Equal expr1 expr2,restTokens2)
                Nothing -> Nothing
        result -> result


-- aritemetic expr

parseIntOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenExpr ((Token TOK_INT (TokenInt n) _ ) : restTokens) =
    Just (Num $ fromIntegral n, restTokens)
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
                    Just (OpAdd expr1 expr2,restTokens2)
                Nothing -> Nothing
        result -> result
