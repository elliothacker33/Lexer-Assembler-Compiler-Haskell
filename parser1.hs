{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Compiler
import Lexer

data Stm =
    IfThenElse Bexp [Stm] [Stm]
    | StmLoop Bexp [Stm]
    | NewVar String Aexp
    deriving (Show)
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

--compile :: [Stm] -> Code
--compile (Token TOK_SPECIAL TokenIf p:rest)=
        
--compile (lexer "if True Then u := 1 + 1 else  u := 2")
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


parse :: [Token] -> [Stm]
parse tokens = 
    case parseT tokens of
        Just (statements, []) ->
            statements
        Just (statements,s) ->
            parse s


parseT :: [Token] -> Maybe ([Stm], [Token])
parseT(Token TOK_SPECIAL TokenIf p:rest)=
    case parseBoolOrParenOrEqualOrLeExpr r1 of
        Just (condition,[]) ->
            case parseT rest1 of
                Just (code,(Token _ TokenElse _):rest2)->
                    case parseT rest2 of
                        Just (codeelse,(Token _ TokenElse _):rest3)->
                            Just ([IfThenElse condition code codeelse],rest3)
                Nothing -> Nothing
        Nothing -> error "expected boolean function after if\n" 
    where
        (r1, rest1) = span (findToken TokenThen) rest

parseT(Token TOK_IDENT (TokenIdent value) p : Token _ TokenAssign _ :rest)=
    case parseSumOrSubOrProdOrIntOrPar r1 of
        Just (ex1,[]) ->
            Just ([NewVar value ex1 ],rest) 
        Nothing -> error "expected boolean function after if\n" 
    where
        (r1, rest1) = span (findToken TokenEndOfStatement) rest
-- work in progress
parseT(Token _ TokenWhile p :rest)=
    case parseBoolOrParenOrEqualOrLeExpr r1 of
        Just (coditicion,[]) ->
            case parseT rest1 of
                Just (loop,rest4) ->
                    Just([StmLoop coditicion loop ],rest4) 
                Nothing -> error "expected a do before end\n"
        Nothing -> error "expected boolean function after loop\n" 
    where
        (r1, rest1) = span (findToken TokenDo) rest

parseT(Token TOK_IDENT (TokenIdent value) p :rest)=
    case parseSumOrSubOrProdOrIntOrPar r1 of
        Just (ex1,[]) ->
            Just([NewVar value ex1 ] ,rest)
        Nothing -> error "expected boolean function after if\n" 
    where
        (r1, rest1) = span (findToken TokenEndOfStatement) rest

parseT(Token _ TokenOpenParenthesis p : Token _ TokenAssign _ :rest)=
    Just(parse (reverse r1) ++ parse (reverse rest1),rest)
    where
        -- last occurence
        (rest1, r1) = span (findToken TokenEndOfStatement) (reverse rest)
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
