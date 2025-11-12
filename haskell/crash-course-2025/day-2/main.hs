import Data.List (nub, delete)

manutdLegend :: Int -> String
manutdLegend 7 = "Eric Cantona"
manutdLegend 11 = "Ryan Giggs"
manutdLegend 1 = "Edwin van der Saar"
manutdLegend _ = "No Legend"

-- factorial :: (Eq t, Num t) => t -> t
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

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

{-- Guard --}
-- grade :: (Ord a, Num a) => a -> Char
grade :: Int -> Char
grade score
  | score > 90 = 'A'
  | score > 80 = 'B'
  | score > 70 = 'C'
  | otherwise  = 'F'

-- factorial' :: (Eq t, Num t) => t -> t
factorial' :: Int -> Int
factorial' n
  | n > 0     = n * factorial' (n - 1)
  | n == 0    = 1
  | otherwise = error "factorial': n >= 0 only"

{-- If-Then-Else --}
-- factorial'' :: (Eq t, Num t) => t -> t
factorial'' :: Int -> Int
factorial'' n =
  if n == 0
    then 1
    else n * factorial'' (n - 1)

{-- Case ... of --}
-- factorial''' :: (Eq t, Num t) => t -> t
factorial''' :: Int -> Int
factorial''' n = case n of
  0 -> 1
  _ -> n * factorial''' (n - 1)

countUniqueWords :: String -> Int
countUniqueWords = count . unique . words
  where
    unique = nub
    count  = length

