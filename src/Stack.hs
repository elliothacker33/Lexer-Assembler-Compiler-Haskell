module Stack (Stack, -- exportar o tipo
push, pop, top, -- e as operações
createEmptyStack, isEmpty) where

-- Data stack 
data Stack a = Stk [a] -- implementação usando listas

-- Push elements to the stack
push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

-- Retrieve/Pop elements out of the stack
pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

-- Top element in the stack
top :: Stack a -> a
top (Stk (x:_)) = x

-- Create empty stack (no elements)
createEmptyStack :: Stack a
createEmptyStack = Stk []

-- Check if stack has no elements
isEmpty :: Stack a -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False
