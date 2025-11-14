qsort :: (Ord a) => [a] -> [a]
qsort []       = []
qsort (x:xs)   = qsort (lessThan x) ++ [x] ++ qsort (atLeast x)
  where
    lessThan n = [y | y <- xs, y < n]
    atLeast n  = [y | y <- xs, y >= n]
