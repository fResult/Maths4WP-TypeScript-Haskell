-- Simple Stack of Int to demonstrate State Monad
newtype Stack = Stack [Int] deriving (Show, Eq)

pop :: Stack -> (Int, Stack)
pop (Stack (x:xs)) = (x, Stack xs)

push :: Int -> Stack -> ((), Stack)
push x (Stack xs) = ((), Stack (x:xs))

stack :: Stack
stack = Stack [1..10]

-- popSecond :: Stack -> (Int, Stack)
popSecond :: Stack -> (Int, Stack)
popSecond stack =
  let (val, newStack1) = pop stack
      (finalVal, newStack2)   = pop newStack1
      ((), finalStack) = push val newStack2
  in (finalVal, finalStack)

manipulateStack :: Stack -> (Int, Stack)
manipulateStack s0 =
  let (_, s1)  = pop s0
      ((), s2) = push 3 s1
      ((), s3) = push 4 s2
  in pop s3
