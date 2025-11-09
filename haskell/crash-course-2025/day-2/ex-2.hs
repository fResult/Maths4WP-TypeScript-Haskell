insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert x (insertionSort xs)


mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    half  = div (length xs) 2
    left  = take half xs
    right = drop half xs

insert :: (Ord a) => a -> [a] -> [a]
insert = undefined

merge :: (Ord a) => [a] -> [a] -> [a]
merge = undefined
