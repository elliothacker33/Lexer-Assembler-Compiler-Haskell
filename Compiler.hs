module Compiler where
import Stack
import Parser
-- instruções da máquina virtual

data Inst =
    Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
    Branch Code Code | Loop Code Code
    deriving Show
type Code = [Inst]
data Storeddata= TT | FF | N Integer
    deriving (Eq, Show)
-- a configuração da máquina
-- a pilha é uma lista de valores
-- o código é uma lista de instruções

type Stored = (String,Storeddata)
type State = [Stored]

createEmptyState:: [(String,Storeddata)]
createEmptyState = []
{-
compile :: Expr -> Code
compile (Num n) = [Push n]
compile (OpAdd e1 e2)
    = compile e1 ++ compile e2 ++ [Add]
compile (OpMult e1 e2)
    = compile e1 ++ compile e2 ++ [Mult]
-}
findStored:: State->String->Storeddata
findStored [] s = error ("Value with key " ++ s ++" Not found")
findStored ((x,stored):l) s
    | x == s = stored
    | otherwise = findStored l s

addStored:: State->(String,Storeddata)->State
addStored [] new = [new]
addStored ((x,stored):l) (new_s,new_stored)
    | x == new_s = (new_s,new_stored):l
    | otherwise = (x,stored):addStored l (new_s,new_stored)

exec :: ( Code,Stack Storeddata,State) -> ( Code,Stack Storeddata,State)
exec ([],stack, stored) = ([],stack, stored)
exec ( Push v:code,stack,stored)=exec ( code,push (N v) stack,stored)
exec ( Tru:code,stack,stored)= ( code,push TT stack,stored)
exec ( Fals:code,stack,stored)= ( code,push FF stack,stored)

exec (Neg:code,stack, stored)
    | top stack == TT = ( code,push FF (pop stack),stored)
    | top stack == FF = ( code,push TT (pop stack),stored)
    | otherwise = error ("not bollean on NEG instead" ++ show (top stack))
exec (And:code,stack,stored)
    | isEmpty stack || isEmpty stack1 = error "Not Enought elements in stack"
    | (top stack /= FF && top stack /= TT ) || (top stack1 /= FF && top stack1 /= TT ) = error ("And operantor only works between booleans not with " ++ show (top stack) ++ " and " ++ show (top stack1))
    | otherwise = (code,push ( andop (top stack) (top stack1)) stack2,stored)
    where
        stack1 = pop stack
        stack2 = pop stack1
        andop TT TT = TT
        andop _ _ = FF

exec (Store s:code,stack, stored)= (code,pop stack, addStored stored (s,top stack))

exec (Branch c1 c2:code,stack, stored)
    | top stack == TT = exec (c1 ++ code,pop stack, stored)
    | top stack == FF = exec ( c2 ++ code,pop stack,stored)
    | otherwise = error ("not bollean on Branch " ++ show (top stack))
exec (Noop:code,stack, stored)=
    exec (code,stack,stored)

exec (Loop c1 c2:code,stack,stored)=
    let loopcode = c2++[Loop c1 c2] in
        let code1 = (Branch  loopcode [Noop]):code  in
            let code2 = c1 ++ code1 in
                (code2 ,stack, stored)

exec ( Equ:code,stack,stored)=
    let v1 = top stack in
        let v2 = top (pop stack ) in
            if v1 == v2 then ( code,push TT (pop (pop stack)),stored)
            else (code,push FF (pop (pop stack)),stored)

exec (Le:code,stack, stored)=
    let N v1 = top stack in
        let N v2 = top (pop stack ) in
            if v1 <= v2 then ( code,push TT (pop (pop stack)),stored)
            else ( code,push FF (pop (pop stack)),stored)


exec (Fetch s:code,stack, stored)=
        let storedvalue = findStored stored s in
            ( code,push storedvalue stack,stored)
exec (Add:code,stack, stored)=
    let N v1 = top stack in
        let N v2 = top (pop stack ) in
            ( code,push (N (v1+v2)) (pop (pop stack)),stored)

exec (Mult:code,stack, stored)=
    let N v1 = top stack in
        let N v2 = top (pop stack ) in
            ( code,push (N (v1*v2)) (pop (pop stack)),stored)

exec (Sub:code,stack, stored)=
    let N v1 = top stack in
        let N v2 = top (pop stack ) in
            ( code,push (N (v1-v2)) (pop (pop stack)),stored)

--state2Str :: State -> String
--state2Str (_,_,store) = listToString store
storeddataToString :: Storeddata -> String
storeddataToString TT = "True"
storeddataToString FF = "False"
storeddataToString (N n) = show n

stack2Str :: Stack Storeddata -> String
stack2Str stack
    | isEmpty stack = ""
    | isEmpty (pop stack) = storeddataToString (top stack)
    | otherwise = storeddataToString (top stack) ++ "," ++ stack2Str (pop stack)

insert1 :: (String, Storeddata) -> [(String, Storeddata)] -> [(String, Storeddata)]
insert1 c [] = [c]
insert1 (s1,d1) ((s,d):xs) 
    | s >= s1 = ((s1,d1):(s,d):xs)  
    | s < s1 = [(s,d)] ++ (insert1 (s1,d1) xs)

mysort :: State -> State
mysort l = foldr insert1 [] l

state2Str :: State -> String
state2Str s = state2Strtmp (mysort s)

state2Strtmp :: State -> String
state2Strtmp [] = ""
state2Strtmp [(s,x)] = s ++ "=" ++ storeddataToString x
state2Strtmp ((s,x):l) = s ++ "=" ++ storeddataToString x ++ "," ++ state2Str l

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


run :: ( Code,Stack Storeddata,State) -> (Code,Stack Storeddata,State)
run ([],stack,stored) = ([],stack,stored)
run s = run (exec s)



