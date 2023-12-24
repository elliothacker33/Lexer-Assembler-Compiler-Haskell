module Compiler where
import Stack
-- instruções da máquina virtual
data Expr = OpAdd Expr Expr
    | OpMult Expr Expr
    | Num Integer
    deriving (Eq, Show)

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
compile :: Expr -> Code
compile (Num n) = [Push n]
compile (OpAdd e1 e2)
    = compile e1 ++ compile e2 ++ [Add]
compile (OpMult e1 e2)
    = compile e1 ++ compile e2 ++ [Mult]

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

run :: ( Code,Stack Storeddata,State) -> (Code,Stack Storeddata,State)
run ([],stack,stored) = ([],stack,stored)
run s = run (exec s)




testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
    where (_,stack,state) = run (code,createEmptyStack,createEmptyState)

-- tests
--testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
--testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
--testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
--testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
--testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
--testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
--testAssembler [Push (-20),Push (-21), Le] == ("True","")
--testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
--testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store"fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")