-- Exercise 1: Self-implementation of Data.List functions
import Data.Foldable (elem, notElem)

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
map' = undefined

-- <12>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:concatMap
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' = undefined

-- <13>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:any
any' :: (a -> Bool) -> [a] -> Bool
any' = undefined

-- <14>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:all
all' :: (a -> Bool) -> [a] -> Bool
all' = undefined

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

-- <18>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:product
product' :: Num a => [a] -> a
product' = undefined

-- <19>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:maximum
maximum' :: Ord a => [a] -> a
maximum' = undefined

-- <20>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:minimum
minimum' :: Ord a => [a] -> a
minimum' = undefined

-- <21>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:iterate
iterate' :: (a -> a) -> a -> [a]
iterate' = undefined

-- <22>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:repeat
repeat' :: a -> [a]
repeat' = undefined

-- <23>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:replicate
replicate' :: Int -> a -> [a]
replicate' = undefined

-- <24>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:cycle
cycle' :: [a] -> [a]
cycle' = undefined

-- <25>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:take
take' :: Int -> [a] -> [a]
take' = undefined

-- <26>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:drop
drop' :: Int -> [a] -> [a]
drop' = undefined

-- <27>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:takeWhile
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' = undefined

-- <28>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:dropWhile
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' = undefined

-- <29>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:span
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' = undefined

-- <30>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:elem
elem' :: Eq a => a -> [a] -> Bool
elem' = undefined

-- <31>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:zip
zip' :: [a] -> [b] -> [(a, b)]
zip' = undefined

-- <32>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:unzip
unzip' :: [(a, b)] -> ([a], [b])
unzip' = undefined

-- <33>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:delete
delete' :: Eq a => a -> [a] -> [a]
delete' = undefined

-- <34>
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:intersect
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' = undefined

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
