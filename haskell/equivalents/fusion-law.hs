double :: Int -> Int
double = (*2)

sumDouble :: [Int] -> Int
sumDouble = (double . sum)
-- sumDouble [1..10]
---- 110

sumDouble' :: [Int] -> Int
sumDouble' = foldr ((+) . double) 0

lengthConcat :: [[Int]] -> Int
lengthConcat = length . concat
-- lengthConcat [[1..10], [16..20]]
---- 15
