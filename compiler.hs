import Stack
-- instruções da máquina virtual
data Expr = OpAdd Expr Expr
    | OpMult Expr Expr
    | Num Integer
    deriving (Eq, Show)

data Inst =
    Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | State String | Noop |
    Branch Code Code | Loop Code Code
    deriving Show
type Code = [Inst]
data Storeddata= TT | FF | N Integer 
    deriving (Eq, Show)
-- a configuração da máquina
-- a pilha é uma lista de valores
-- o código é uma lista de instruções

type Stored = (String,Storeddata)
type State = (Stack Storeddata, Code,[Stored])

createEmptyStored:: [(String,Storeddata)]
createEmptyStored = []
compile :: Expr -> Code
compile (Num n) = [Push n]
compile (OpAdd e1 e2)
    = compile e1 ++ compile e2 ++ [Add]
compile (OpMult e1 e2)
    = compile e1 ++ compile e2 ++ [Mult]

findStored:: [Stored]->String->([Stored],Storeddata)
findStored [] _ = error "Value not found"
findStored ((x,stored):l) s
    | x == s =(l,stored)
    | otherwise = 
        let (list , result) = findStored l s in
            ((x,stored):list,result)
exec :: State -> State
exec (stack, [],stored) = (stack, [],stored)

exec (stack, Push v:code,stored)=exec (push (N v) stack, code,stored)
exec (stack, Tru:code,stored)= (push TT stack, code,stored)
exec (stack, Fals:code,stored)= (push FF stack, code,stored)
exec (stack, Neg:code,stored)
    | top stack == TT = (push FF (pop stack), code,stored)
    | top stack == FF = (push TT (pop stack), code,stored)
    | otherwise = error "not bollean on NEG"

exec (stack, State s:code,stored)= (pop stack, code,(s,top stack):stored)

exec (stack, Equ:code,stored)=
    let v1 = top stack in
        let v2 = top (pop stack ) in
            if v1 == v2 then (push TT (pop (pop stack)), code,stored)
            else (push FF (pop (pop stack)), code,stored)

exec (stack, Le:code,stored)=
    let N v1 = top stack in
        let N v2 = top (pop stack ) in
            if v1 <= v2 then (push TT (pop (pop stack)), code,stored)
            else (push FF (pop (pop stack)), code,stored)
                       

exec (stack, Fetch s:code,stored)= 
        let (newstoredvalues,storedvalue) = findStored stored s in
            (push storedvalue stack, code,newstoredvalues)
exec (stack, Add:code,stored)=
    let N v1 = top stack in
        let N v2 = top (pop stack ) in
            (push (N (v1+v2)) (pop (pop stack)), code,stored)

exec (stack, Mult:code,stored)=
    let N v1 = top stack in
        let N v2 = top (pop stack ) in
            (push (N (v1*v2)) (pop (pop stack)), code,stored)

exec (stack, Sub:code,stored)=
    let N v1 = top stack in
        let N v2 = top (pop stack ) in
            (push (N (v1-v2)) (pop (pop stack)), code,stored)

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
state2Str :: [Stored] -> String
state2Str [] = ""
state2Str [(s,x)] = s ++ "=" ++ storeddataToString x
state2Str ((s,x):l) = s ++ "=" ++ storeddataToString x ++ "," ++ state2Str l

runState :: State -> (Stack Storeddata,Code,[Stored])
runState (stack, [],stored) = (stack,[],stored)
runState s = runState (exec s)

tmpio :: Code -> IO()
tmpio e = do
    let (stack,status)=run (e)
    print stack
    print status

run :: Code -> (String,String)
run c =
    let (stack,_,stored) = runState (createEmptyStack,c,[]) in
        (stack2Str stack, state2Str stored)


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
    where (stack,_,state) = runState(createEmptyStack,code,createEmptyStored)
