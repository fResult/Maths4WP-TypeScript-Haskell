sum' :: [Int] -> Int
sum' [] = 0
sum' [x] = x
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []
-- reverse' [] = []
-- reverse' (x:xs) = reverse xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x, y)] ++ zip' xs ys
