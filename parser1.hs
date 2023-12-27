
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
    | GetVar String
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

findFirst :: (a -> Bool) -> [a] -> ([a],[a])
findFirst _ [] = ([],[])
findFirst op (x:xs) 
    | op x = ([],x:xs)
    | otherwise = (x:newList,oldlist)
    where
        (newList,oldlist) = findFirst op xs
compile :: [Stm] -> Code
compile [] = []
compile (IfThenElse boleanexp c1 c2:rest)=
    compB boleanexp ++ [Branch (compile c1) (compile c2)]

compile (StmLoop boleanexp c:rest)=
    [Loop (compB boleanexp) (compile c)] ++ compile rest

compile ( NewVar string exp:rest)=
    compA exp ++ [Store string] ++ compile rest
--compile (lexer "if True Then u := 1 + 1 else  u := 2")
compA :: Aexp -> Code
compA (Num n) = [Push n]
compA  (GetVar s) = [Fetch s]
compA (OpAdd e1 e2)
    = compA e1 ++ compA e2 ++ [Add]
compA (OpMult e1 e2)
    = compA e1 ++ compA e2 ++ [Mult]
compA (OpSub e1 e2)
    = compA e1 ++ compA e2 ++ [Sub]

compB :: Bexp -> Code
compB (Bo False) = [Fals]
compB (Bo True) = [Tru]
compB (Negation b) = compB b ++[Neg]
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
            statements ++ parse s


parseT :: [Token] -> Maybe ([Stm], [Token])
parseT(Token _ TokenEndOfStatement p :rest)=
    parseT rest

parseT [] =
    Just([],[])
parseT(Token t TokenElse p :rest)=
    Just([],Token t TokenElse p :rest)
parseT(Token TOK_KEYWORD TokenIf p:rest)=
    case parseBoolOrParenOrEqualOrLeExpr r1 of
        Just (condition,x) ->
            case parseT $ tail rest1 of
                Just (code,(Token _ TokenElse _):rest2)->
                    case parseT rest2 of
                        Just (codeelse,rest3)->
                            Just ([IfThenElse condition code codeelse],rest3)
                        Nothing -> error "error parsing else statement\n" 
                Just (code,(Token _ TokenEndOfStatement _):(Token _ TokenElse _):rest2)->
                    case parseT rest2 of
                        Just (codeelse,rest3)->
                            Just ([IfThenElse condition code codeelse],rest3)
                        Nothing -> error "error parsing else statement\n" 
                a ->  error $ "error parsing if code1\n" ++ show a 
        a -> error $ "expected boolean function after if\n" ++ show r1
    where
        (r1, rest1) = findFirst (findToken TokenThen) rest
-- parse $ lexer "if x== 3 then x:=3 else y:=3"
parseT(Token TOK_IDENT (TokenIdent value) p : Token _ TokenAssign _ :rest)=
    case parseSumOrSubOrProdOrIntOrPar r1 of
        Just (ex1,[]) ->
            case parseT rest1 of
                Just (ex2,rest2) ->
                    Just((NewVar value ex1) :ex2 ,rest2)

        Just (ex1,x) ->
            error $ "cant work : "++ show x
        Nothing -> error $ "something went wrong when trying to parse declare variable\n" ++ show r1
    where
        (r1, rest1) = findFirst (findToken TokenEndOfStatement) rest


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
        (r1, rest1) = findFirst (findToken TokenDo) rest

parseT(Token _ TokenOpenParenthesis p :rest)=
    case parseT rest of
        Just(ex1,(Token _ TokenClosedParenthesis _):rest1) ->
            Just(ex1,rest1)
        a -> error $ "testing "++ show a -- no closing paren
parseT(Token t TokenClosedParenthesis p :rest)=
    Just([],Token t TokenClosedParenthesis p :rest)

-- bool expr

parseBoolOrParenExpr :: [Token] -> Maybe (Bexp, [Token])
parseBoolOrParenExpr ((Token TOK_BOOL (TokenBool b) _ ) : restTokens) =
    Just (Bo b, restTokens)
    
parseBoolOrParenExpr ((Token TOK_SPECIAL TokenOpenParenthesis _): restTokens1) =
    case parseBoolOrParenOrEqualOrLeExpr restTokens1 of
        Just (expr, (Token TOK_SPECIAL TokenClosedParenthesis _): restTokens2) ->
            Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
parseBoolOrParenExpr tokens = Nothing


parseBoolOrParenOrEqualOrLeExpr:: [Token] -> Maybe (Bexp, [Token])
-- operations that generate bolleans
parseBoolOrParenOrEqualOrLeExpr ((Token TOK_INT (TokenInt value) p ):tokens) =
    case parseSumOrSubOrProdOrIntOrPar $ Token TOK_INT (TokenInt value) p:tokens of
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
        Just (x , y) ->
            error ("error x: " ++ show x ++ " errory:" ++ show y)

parseBoolOrParenOrEqualOrLeExpr ((Token TOK_IDENT (TokenIdent value) p ):tokens) =
    case parseSumOrSubOrProdOrIntOrPar $ (Token TOK_IDENT (TokenIdent value) p ):tokens of
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
        a->
            error ("tokens :"++show a)

-- operations with booleans
parseBoolOrParenOrEqualOrLeExpr tokens =
    case parseBoolOrParenExpr tokens of
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
                    Just (OpAdd expr1 expr2,restTokens2)
                Nothing -> Nothing
        result -> result
-- parse lexer "if x == 3 then x:=x+1;else u:=2"