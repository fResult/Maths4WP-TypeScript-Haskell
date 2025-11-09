# Exercise 2

Sorting algorithms help us arrange data in order.\
We will implement two common sorting algorithms using helper functions.

Write the helper `insert`, and `merge` functions to complete these sorting algorithms:

```hs
insertionsort :: (Ord a) => [a] -> [a]
insertionsort [] = []
insertionsort (x:xs) = insert x (insertionsort xs)
```

```hs
mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
  where
    half  = div (length xs) 2
    left  = take half xs
    right = drop half xs
```

