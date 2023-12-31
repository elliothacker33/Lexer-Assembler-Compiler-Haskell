
import Compiler
import Lexer
import Stack
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
    | IntEqual Aexp Aexp
    | Equal Bexp Bexp
    | AndOp Bexp Bexp
    -- | OrOp Bexp Bexp
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
    compB boleanexp ++ [Branch (compile c1) (compile c2)] ++ compile rest

compile (StmLoop boleanexp c:rest)=
    [Loop (compB boleanexp) (compile c)] ++ compile rest

compile ( NewVar string exp:rest)=
    compA exp ++ [Store string] ++ compile rest
--compile (lexer "if True Then u := 1 + 1 ;else  u := 2")
compA :: Aexp -> Code
compA (Num n) = [Push n]
compA  (GetVar s) = [Fetch s]
compA (OpAdd e1 e2)
    = compA e2 ++ compA e1 ++ [Add]
compA (OpMult e1 e2)
    = compA e2 ++ compA e1 ++ [Mult]
compA (OpSub e1 e2)
    = compA e2 ++ compA e1 ++ [Sub]

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


parse :: [Token] -> [Stm]
parse tokens = 
    case parseT tokens of
        Just (statements, []) ->
            statements
        Just (statements,s) ->
            statements ++ parse s


parseT :: [Token] -> Maybe ([Stm], [Token])

parseT [] =
    Just([],[])

parseT(Token t TokenDo p :rest)=
    Just([],Token t TokenDo p :rest)

parseT(Token t TokenElse p :rest)=
    Just([],Token t TokenElse p :rest)

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

-- parse $ lexer "if x== 3 then x:=3; else y:=3;"
parseT(Token TOK_IDENT (TokenIdent value) p : Token _ TokenAssign _ :rest)=
    case parseSumOrSubOrProdOrIntOrPar rest of
        Just (ex1,(Token _ TokenEndOfStatement _ ):rest1) ->
            Just([NewVar value ex1] ,rest1)
        Just (ex1,x) ->
            error $ "Expected end of statement but found : "++ show ( head x)
        Nothing -> error $ "something went wrong when trying to parse declare variable\n" ++ show rest


-- work in progress
parseT(Token _ TokenWhile p :rest)=
    case parseBoolOrParenOrEqualOrLeOrNotOrAndExpr rest of
        Just (coditicion,(Token _ TokenDo _) :rest2) ->
            case parseElseOrDoStatement rest2 of
                Just (loop,rest4) ->
                    Just([StmLoop coditicion loop ],rest4) 
                Nothing -> error "expected a do before end\n"
        _ -> error "error while parsing while loop\n" 

parseT (t:rest)=
    error $ "cound't parse :" ++ show t
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
-- bool expr

parseNotOrBoolOrParenOrIntcompExpr :: [Token] -> Maybe (Bexp, [Token])
parseNotOrBoolOrParenOrIntcompExpr ((Token TOK_BOOL (TokenBool b) _ ) : restTokens) =
    Just (Bo b, restTokens)

parseNotOrBoolOrParenOrIntcompExpr ((Token TOK_SPECIAL TokenOpenParenthesis _): restTokens1) =
    case parseBoolOrParenOrEqualOrLeOrNotOrAndExpr restTokens1 of
        Just (expr, (Token TOK_SPECIAL TokenClosedParenthesis _): restTokens2) ->
            Just (expr, restTokens2)
        Just _ -> Nothing -- no closing paren
    
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


parseBoolOrParenOrEqualOrLeOrNotExpr:: [Token] -> Maybe (Bexp, [Token])


-- operations with booleans
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
-- so that i can make a function that does aritemetic and bollean expressions so that i can do if x+2==3;

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
        a->
            error ("tokens :"++show a)

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
                    Just (OpSub expr1 expr2,restTokens2)
                Nothing -> Nothing
        result -> result
-- parseBoolOrParenOrEqualOrLeOrNotExpr $ lexer "! True"
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
    where (_,stack,state) = run(compile (parse $ lexer programCode), createEmptyStack, createEmptyState)
-- parse $ lexer "if ! (x == 3) then x:=x+1;else u:=2;"
-- tests
main = do
    print b1
    print b2
    print b3
    print b4
    print b5
    print b6
    print b7
    print b8
    print b9
    print b10
    print b11
    print b12
    print b13
    where
        b1 = testParser "x := 5; x := x - 1;" == ("","x=4")
        b2 = testParser "x := 0 - 2;" == ("","x=-2")
        b3 = testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
        b4 = testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
        b5 = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
        b6 = testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
        b7 = testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
        b8 = testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
        b9 = testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
        b10 = testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
        b11 = testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
        b12 = testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
        b13 = testParser "x := 44; if x <= 43 then x := 1; else ( x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")