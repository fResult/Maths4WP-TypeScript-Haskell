-- data [] a = [] | a : [a]
-- instance Functor [] where
-- fmap = map

data Box a = EmptyBox | Box a deriving (Show, Eq, Ord)

-- Some containers are just ‘Box’ containing another structure
newtype Stack a = Stack [a] deriving (Show, Eq, Ord)

newtype Queue a = Queue [a] deriving (Show, Eq, Ord)

data Queue' a = Empty | Valua a (Queue a) deriving (Show, Eq, Ord)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)

instance Functor Stack where
  fmap :: (a -> b) -> Stack a -> Stack b
  fmap f (Stack xs) = Stack (map f xs)

instance Functor Queue where
  fmap :: (a -> b) -> Queue a -> Queue b
  fmap f (Queue xs) = Queue (map f xs)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f EmptyTree = EmptyTree
  fmap f (Node x tl tr) = Node (f x) (fmap f tl) (fmap f tr)

stkPush :: a -> Stack a -> Stack a
stkPush x (Stack xs) = Stack (x : xs)
stkPop :: Stack a -> Stack a
stkPop (Stack (x : xs)) = Stack xs
stkPeek :: Stack a -> a
stkPeek (Stack (x : _)) = x
stkAsList :: Stack a -> [a]
stkAsList (Stack xs) = xs

myTree :: Tree Char
