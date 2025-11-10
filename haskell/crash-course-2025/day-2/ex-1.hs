-- Exercise 1: Self-implementation of Data.List functions
import Data.Foldable (elem, notElem, foldr)
import Data.Bifunctor (bimap)
import qualified Data.Map as Map (fromList, member, lookup)

-- <1>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:length
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

-- <2>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:group
group' :: Eq a => [a] -> [[a]]
group' = undefined

-- <3>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:nub
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs)
  | x `isNotIn` xs = x : nub' xs
  | otherwise      = nub' xs
    where isNotIn = notElem

-- <4>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []  = []
filter' f (x:xs)
  | f x       = x : filter f xs
  | otherwise = filter f xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f xs = [ x | x <- xs, f x ]

-- <5>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:head
head' :: [a] -> a
head' []     = error "head': empty list"
head' (x:_) = x

-- <6>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:tail
tail' :: [a] -> [a]
tail' []     = error "tail': empty list"
tail' (_:xs) = xs

-- <7>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:init
init' :: [a] -> [a]
init' = undefined

-- <8>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:last
last' :: [a] -> a
last' = undefined

-- <9>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:reverse
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

-- <10>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:concat
concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

-- <11>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [ f x | x <- xs ]

-- <12>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:concatMap
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' = undefined

-- <13>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:any
any' :: (a -> Bool) -> [a] -> Bool
any' _ []     = False
any' f (x:xs) = f x || any' f xs

-- <14>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:all
all' :: (a -> Bool) -> [a] -> Bool
all' _ []     = True
all' f (x:xs) = f x && all' f xs

-- <15>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:and
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs)
  | not x      = False
  | otherwise  = and' xs

and'' :: [Bool] -> Bool
and'' = notElem False

-- <16>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:or
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs)
  | x = True
  | otherwise = or' xs

or'' :: [Bool] -> Bool
or'' = elem True

-- <17>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:sum
sum' :: Num a => [a] -> a
sum' []     = 0
sum' [x]    = x
sum' (x:xs) = x + sum' xs

sum'' :: Num a => [a] -> a
sum'' = foldr (+) 0

-- <18>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:product
product' :: Num a => [a] -> a
product' [] = 1
product' [x] = x
product' (x:xs) = x * product' xs

product'' :: Num a => [a] -> a
product'' = foldr (*) 1

-- <19>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:maximum
maximum' :: Ord a => [a] -> a
maximum' []   = error "maximum': empty list"
maximum' [x]  = x
maximum' (x:y:xs)
  | x > y     = maximum' (x:xs)
  | otherwise = maximum' (y:xs)

maximum'' :: Ord a => [a] -> a
maximum'' [] = error "maximum'': empty list"
maximum'' [x] = x
maximum'' (x:xs) = max' x (maximum'' xs)
  where
    max' y z
      | y > z     = y
      | otherwise = z

-- <20>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:minimum
minimum' :: Ord a => [a] -> a
minimum' []  = error "minimum': empty list"
minimum' [x] = x
minimum' (x:y:xs)
  | x < y     = minimum' (x:xs)
  | otherwise = minimum' (y:xs)

minimum'' :: Ord a => [a] -> a
minimum'' []     = error "minimum'': empty list"
minimum'' [x]    = x
minimum'' (x:xs) = min' x (minimum'' xs)
  where
    min' y z
      | y < z     = y
      | otherwise = z

-- <21>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:iterate
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f step
  where step = f x

-- <22>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:repeat
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- <23>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:replicate
replicate' :: Int -> a -> [a]
replicate' n x = take' n $ repeat' x

replicate'' :: Int -> a -> [a]
replicate'' 0 _ = []
replicate'' n x = x : replicate'' (n - 1) x

-- <24>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:cycle
cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs

-- <25>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:take
take' :: Int -> [a] -> [a]
take' _ []    = []
take' n (x:xs)
  | n <= 0    = []
  | otherwise = x : take' (n - 1) xs

-- <26>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:drop
drop' :: Int -> [a] -> [a]
drop' _ []    = []
drop' n (x:xs)
  | n <= 0    = x:xs
  | otherwise = drop' (n - 1) xs

-- <27>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:takeWhile
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | f x         = x : takeWhile' f xs
  | otherwise   = []

-- <28>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:dropWhile
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []     = []
dropWhile' f (x:xs) = if f x then dropWhile' f xs else x:xs

-- <29>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:span
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _ [] = ([], [])
span' f xs = (takeWhile' f xs, dropWhile' f xs)

-- <30>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:elem
elem' :: Eq a => a -> [a] -> Bool
elem' _ []       = False
elem' e (x:xs) = x == e || elem' e xs

elem'' :: Eq a => a -> [a] -> Bool
elem'' e xs = or' $ map' isEqualE xs
  where isEqualE x = e == x

-- <31>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:zip
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- <32>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:unzip
unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip'  ((x,y):xs) = (x:fst unzipped, y:snd unzipped)
  where unzipped = unzip' xs

unzip'' :: [(a, b)] -> ([a], [b])
unzip'' [] = ([], [])
unzip'' ((x,y):xs) = bimap (x:) (y:) unzipped
  where unzipped = unzip' xs

-- <33>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:delete
delete' :: Eq a => a -> [a] -> [a]
delete' _ []     = []
delete' y (x:xs)
  | y == x    = delete' y xs
  | otherwise = x : delete' y xs

delete'' :: Eq a => a -> [a] -> [a]
delete'' x = filter' (/= x)

-- <34>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:intersect
intersect' :: (Eq a, Ord a) => [a] -> [a] -> [a]
intersect' _ [] = []
intersect' [] _ = []
intersect' xs ys = filter' (`Map.member` hashMap) ys
  where
    hashMap = Map.fromList $ map' (,True) xs

-- <35>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:intersperse
intersperse' :: a -> [a] -> [a]
intersperse' = undefined

-- <36>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:intercalate
intercalate' :: [a] -> [[a]] -> [a]
intercalate' = undefined

-- <37>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:permutations
permutations' :: [a] -> [[a]]
permutations' = undefined
