import Control.Monad.State

newtype Stack = Stack [Int] deriving (Show, Eq)

pop :: Stack -> (Int, Stack)
pop (Stack (x:xs)) = (x, Stack xs)

push :: Int -> Stack -> ((), Stack)
push x (Stack xs) = ((), Stack (x:xs))

stack :: Stack
stack = Stack [1..10]

pop' :: State Stack Int
pop' = state $ \(Stack (x:xs)) -> (x, Stack xs)

push' :: Int -> State Stack ()
push' a = state $ \(Stack xs) -> ((), Stack (a:xs))

removeSecond :: Stack -> ((), Stack)
removeSecond stack =
  let (val, newStack1) = pop stack
      (_, newStack2)   = pop newStack1
  in push val newStack2

removeSecond' :: State Stack ()
removeSecond' = do
  val <- pop'
  pop'
  push' val

stackManip :: Stack -> (Int, Stack)
stackManip s0 =
  let (_, s1) = pop s0
      ((), s2) = push 3 s1
      ((), s3) = push 4 s2
  in pop s3


stackManip' :: State Stack Int
stackManip' = do
  pop'
  push' 3
  push' 4
  pop'


-- runState removeSecond' stack