-- Simple Stack of Int to demonstrate State Monad
newtype Stack = Stack [Int] deriving (Show, Eq)

pop :: Stack -> (Int, Stack)
pop (Stack (x:xs)) = (x, Stack xs)

push :: Int -> Stack -> ((), Stack)
push x (Stack xs) = ((), Stack (x:xs))

statePop :: State' Stack Int
statePop = state' $ \(Stack (x:xs)) -> (x, Stack xs)

statePush :: Int -> State' Stack ()
statePush x = state' $ \(Stack xs) -> ((), Stack (x:xs))

stack :: Stack
stack = Stack [1..10]

-- popSecond :: Stack -> (Int, Stack)
popSecond :: Stack -> (Int, Stack)
popSecond stack =
  let (val, newStack1) = pop stack
      (finalVal, newStack2)   = pop newStack1
      ((), finalStack) = push val newStack2
  in (finalVal, finalStack)

statePopSecond :: State' Stack Int
statePopSecond =
  statePop >>= (\val ->
    statePop >>= (\finalVal ->
      statePush val >>= (\_ ->
        return finalVal)))

manipulateStack :: Stack -> (Int, Stack)
manipulateStack s0 =
  let (_, s1)  = pop s0
      ((), s2) = push 3 s1
      ((), s3) = push 4 s2
  in pop s3

newtype State' s a = State' { runState' :: s -> (a, s) }

instance Functor (State' s) where
  fmap :: (a -> b) -> State' s a -> State' s b
  fmap f (State' g) = State' $ \s ->
    let (a, s') = g s
    in (f a, s')

instance Applicative (State' s) where
  pure :: a -> State' s a
  pure x = State' { runState' = \s -> (x, s) }

  (<*>) :: State' s (a -> b) -> State' s a -> State' s b
  (State' hf) <*> (State' hg) = State' $ \s ->
    let (f, s')  = hf s
        (x, s'') = hg s'
    in (f x, s'')

instance Monad (State' s) where
  (>>=) :: State' s a -> (a -> State' s b) -> State' s b
  (State' h) >>= f = State' $ \s ->
    let (a, s')    = h s
        (State' g) = f a
    in g s'

class Monad m => MonadState' s m where
  state' :: (s -> (a, s)) -> m a

-- state' helper
instance MonadState' s (State' s) where
  state' :: (s -> (a, s)) -> State' s a
  state' f = State' { runState' = f }
