import Data.List

primeToN :: Int -> [Int]
-- primeToN 0 = []
primeToN n = [x | x <- [1..n], isPrime x] where
    isPrime y = [z | z <- [1..y], mod y z == 0] == [1, y] :: Bool

filterByDivisibleByX :: Int -> [Int]
filterByDivisibleByX x = [y | y <- [1..x], mod x y == 0]

isPrime :: Int -> Bool
isPrime x = filterByDivisibleByX x == [1, x]

sievePrimeToN :: Int -> [Int]
sievePrimeToN n = sieve [2..n] where
    sieve [] = []
    sieve (x:xs) = x : sieve [y | y <- xs, mod y x /= 0]

permute :: (Eq a) => [a] -> [[a]]
permute [] = [[]]
permute xs = [x:ys | x <- xs, ys <- permute (delete x xs)]
