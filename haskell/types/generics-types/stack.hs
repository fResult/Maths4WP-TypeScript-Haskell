data Stack a = Stack [a] deriving Show

createStack :: a -> Stack a
createStack x = Stack [x]

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> Stack a
pop (Stack []) = error "Stack underflow!"
pop (Stack  (_:xs)) = Stack xs

top :: Stack a -> a
top (Stack []) = error "Stack is empty!"
top (Stack (x:_)) = x
