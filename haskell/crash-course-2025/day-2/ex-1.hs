-- Exercise 1: Self-implementation of Data.List functions

-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:length
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs
