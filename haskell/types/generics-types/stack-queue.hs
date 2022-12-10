import Data.List (delete, last)

{- StackLeft Implementation START -}
data StackLeft a = StackLeft [a] deriving Show

createStackLeft :: [a] -> StackLeft a
createStackLeft xs = StackLeft xs

pushStackLeft :: a -> StackLeft a -> StackLeft a
pushStackLeft x (StackLeft xs) = StackLeft (x:xs)

popStackLeft :: StackLeft a -> StackLeft a
popStackLeft (StackLeft []) = error "Can't pop empty stack"
popStackLeft (StackLeft (_:xs)) = StackLeft xs

emptyStackLeft :: StackLeft a -> StackLeft a
emptyStackLeft (StackLeft _) = StackLeft []

topStackLeft :: StackLeft a -> a
topStackLeft (StackLeft []) = error "Can't top empty Stack"
topStackLeft (StackLeft (x:_)) = x
{- StackLeft Implementation END -}

{- StackRight Implementation START -}
data StackRight a = StackRight [a] deriving Show

createStackRight :: [a] -> StackRight a
createStackRight xs = StackRight xs

pushStackRight :: a -> StackRight a -> StackRight a
pushStackRight x (StackRight xs) = StackRight (xs ++ [x])

popStackRight :: (Eq a) => StackRight a -> StackRight a
popStackRight (StackRight []) = error "Can't pop empty stack"
popStackRight (StackRight xs) = StackRight (delete (last xs) xs)

topStackRight :: StackRight a -> a
topStackRight (StackRight []) = error "Can't top empty stack"
topStackRight (StackRight xs) = last xs

emptyStackRight :: StackRight a -> StackRight a
emptyStackRight (StackRight _) = StackRight []

{- StackRight Implementation END -}

data QueueLeft a = QueueLeft [a] deriving Show
data QueueRight a = QueueRight [a] deriving Show