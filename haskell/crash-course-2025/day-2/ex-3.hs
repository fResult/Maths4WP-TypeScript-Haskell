-- Exercise 3: Stalin Sort Functions

stalinSort :: Ord a => [a] -> [a]
stalinSort []  = []
stalinSort [x] = [x]
stalinSort (x:y:xs)
  | x <= y      = x : stalinSort (y:xs)
  | otherwise   = stalinSort (x:xs)
