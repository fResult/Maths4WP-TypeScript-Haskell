manutdLegend :: Int -> String
manutdLegend 7 = "Eric Cantona"
manutdLegend 11 = "Ryan Giggs"
manutdLegend 1 = "Edwin van der Saar"
manutdLegend _ = "No Legend"

-- sum' :: Num a => [a] -> a
sum' :: [Int] -> Int
sum' []     = 0
sum' [x]    = x
sum' (x:xs) = x + sum' xs

count :: [a] -> Int
count []     = 0
count [_]    = 1
count (x:xs) = 1 + count xs

-- λ> count "hello"
-- 5
-- λ> count [1..10]
-- 10
-- λ> import Data.Char
-- λ> :type [isSpace, isDigit, isLetter]
-- λ> [isSpace, isDigit, isLetter] :: [Char -> Bool]
-- λ> count [isSpace, isDigit, isLetter]
-- λ> 3


