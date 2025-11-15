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
