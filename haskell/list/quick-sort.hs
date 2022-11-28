quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]

quicksort' :: [Int] -> [Int]
quicksort' [] = []
quicksort' (x:xs) = quicksort' less ++ [x] ++ quicksort' equalOrMore where
    less = [y | y <- xs, y < x]
    equalOrMore = [y | y <- xs, y >= x]
