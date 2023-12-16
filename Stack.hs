module Stack (Stack, -- exportar o tipo
push, pop, top, -- e as operações
createEmptyStack, isEmpty) where
data Stack a = Stk [a] -- implementação usando listas
push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)
pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"
top :: Stack a -> a
top (Stk (x:_)) = x
createEmptyStack :: Stack a
createEmptyStack = Stk []
isEmpty :: Stack a -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False