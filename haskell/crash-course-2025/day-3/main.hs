import Data.List (delete, concatMap)

-- Sorting algorithms
qsort :: (Ord a) => [a] -> [a]
qsort []       = []
qsort (x:xs)   = qsort (lessThan x) ++ [x] ++ qsort (atLeast x)
  where
    lessThan n = [y | y <- xs, y < n]
    atLeast n  = [y | y <- xs, y >= n]

-- Prime number generation (from Day 2 exercises)
primeTo :: Int -> [Int]
primeTo n = sieve [2..n]
  where
    sieve []     = []
    sieve (x:xs) = x : sieve (filter (notDivisible x) xs)
      -- where notDivisible y n = n `mod` y /= 0
      where notDivisible y n = n `rem` y /= 0

-- Alternative prime implementation
primeTo' :: Int -> [Int]
primeTo' n = sieve' [2..n]
  where
    sieve' []     = []
    sieve' (x:xs) = x : sieve' [ y | y <- xs, y `mod` x /= 0 ]
      where notDivisible y n = n `mod` y /= 0
      -- where notDivisible y n = n `rem` y /= 0

-- Permutation algorithms (from Day 2 exercises)
permute :: Eq a => [a] -> [[a]]
permute [] = []
permute xs = [ x:ys | x <- xs, ys <- permute $ delete x xs ]

-- Alternative permutation with interleaving
permute' :: Eq a => [a] -> [[a]]
---- permute' (x:xs) = concat $ map (interleave x) (permute' xs)
---- permute' (x:xs) = (concat . map) (interleave x) (permute' xs)
permute' (x:xs) = concatMap (interleave x) (permute' xs)

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- Partial application examples (add to README?)
applyTo :: a -> (a -> b) -> b
applyTo n f = f n

applyTo5 :: (Int -> a) -> a
applyTo5 = applyTo 5

-- Manual implementation of map (from Day 2 exercise)
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map f xs
