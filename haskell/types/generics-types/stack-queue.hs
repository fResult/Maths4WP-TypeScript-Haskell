-- import Data.List (delete, last)

{-# StackLeft Implementation #-}
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

data StackRight a = StackRight [a] deriving Show
data QueueLeft a = QueueLeft [a] deriving Show
data QueueRight a = QueueRight [a] deriving Show