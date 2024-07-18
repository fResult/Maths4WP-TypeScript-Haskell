import Data.List

add x y = x + y

manutdLegend :: Int -> String
manutdLegend 7 = "Eric Cantona"
manutdLegend 11 = "Ryan Giggs"
manutdLegend 6 = "Rio Ferdinand"
manutdLegend _ = "No Legend"

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

grade :: Int -> Char
grade score
  | score >= 90 = 'A'
  | score >= 80 = 'B'
  | score >= 70 = 'C'
  | score >= 60 = 'D'
  | otherwise = 'F'

factorial' :: Integer -> Integer
factorial' n
  | n == 0 = 1
  | otherwise = n * factorial' (n - 1)


countUniqueWords :: String -> Int
countUniqueWords = count . unique . words
  where
    unique = nub
    count = length

count :: [a] -> Int
count [] = 0
count (_:xs) = 1 + count xs

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' pred (x:xs)
  | pred x    = x : filter' pred xs
  | otherwise = filter' pred xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' pred (x:xs) = if pred x then x:filtered else filtered
  where
    filtered = filter'' pred xs

filter''' :: (a -> Bool) -> [a] -> [a]
filter''' pred = foldr (\x acc -> if pred x then x:acc else acc) []


main :: IO ()
main = do
  print "hello"

a = 1