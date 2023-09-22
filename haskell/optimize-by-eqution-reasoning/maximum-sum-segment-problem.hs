module MaximumSumSegmentProblem where
import Data.List as L (nub)

tails' :: [a] -> [[a]]
tails' []       = [[]]
tails' s@(x:xs) = s : tails' xs

inits' :: [a] -> [[a]]
inits' []     = [[]]
inits' (x:xs) = [] : map (x:) (inits' xs)

segments' :: [a] -> [[a]]
segments' = concat . map inits' . tails'

-- inits' [1,2,3] = [] : map (1:) (inits' [2,3])
--                = [] : map (1:) ([] : map (2:) (inits' [3]))
--                = [] : map (1:) ([] : map (2:) ([] : map (3:) (inits' [])))
--                = [] : map (1:) ([] : map (2:) ([] : map (3:) [[]]))
--                = [] : map (1:) ([] : map (2:) ([] : [[3]]))
--                = [] : map (1:) ([] : map (2:) [[], [3]])
--                = [] : map (1:) ([] : [[2], [2, 3]])
--                = [] : map (1:) [[], [2], [2, 3]]
--                = [] : [[1], [1, 2], [1, 2, 3]]
--                = [[], [1], [1, 2], [1, 2, 3]]

maximumSumSegment :: [Int] -> Int
maximumSumSegment = maximum . map sum . segments'

--- DO Equational Reasoning for maximumSumSegment ---
-- maximumSumSegment = maximum . map sum . concat . map inits . tails
--                   = maximum . concat . map (map sum) . map inits . tails             <-- map f . concat = concat . map (map f) -- Concat Law
--                   = maximum . concat . map ((map sum) . inits) . tails               <-- map g . map f = map (g . f) -- Functor Las
--                   = maximum . map maximum . map ((map sum). inits) . tails           <-- maximum . concat = maximum . map maximum -- FOR non-empty list
--                   = maximum . map (maximum . (map sum . inits)) . tails              <-- map g . map f = map (g . f) -- Functor Las
--                   = maximum . map (maximum . (map (foldl (+) 0) . inits)) . tails    <-- sum = foldl (+) 0
--                   = maximum . map (maximum . scanl (+) 0) . tails                    <-- map (foldl (+) 0) . inits = scanl -- Naive Scanl, but we replace with the more efficient Scanl
------         ∴  maximum . map sum . concat . map inits . tails = maximum . map (maximum . scanl (+) 0) . tails
maximumSumSegment' :: [Int] -> Int
maximumSumSegment' = maximum . map (maximum . scanl (+) 0) . tails'

--- DO Equational Reasoning for maximumSumSegment' ---
-- maximumSumSegment' = maximum . map (maximum . scanl (+) 0) . tails
--                    = maximum . map (maximum . foldr (\x xs -> 0 : map (x+) xs) [0]) . tails     <-- scanl @ e = foldr (\x xs -> e : map (x@) xs) [e]))
--                    = foldr max . map (maximum . foldr (\x xs -> 0 : map (x+) xs) [0]) . tails   <-- maximum = foldr max
--                    = foldr1 max . map (maximum . foldr (\x xs -> 0 : map (x+) xs) [0]) . tails  <-- foldr = foldr1 -- like foldr but list must have at least one element
--                    =

--- DO Calculation for Scanl
-- scanl (+) 0 [x, y, z] = [0, 0+x, (0+x)+y, ((0+x)+y)+z]
--                       = [0, x, x+y, x+y+z]
--                       = 0 : map (x+) [0, y, y+z]
--                       = 0 : map (x+) (scanl (+) 0 [y, z])
--                       = 0 : map (x+) (0 : map (y+) (scanl (+) 0 [z]))
--                       = 0 : map (x+) (0 : map (y+) (0 : map (z+) (scanl (+) 0 [])))
--                       = 0 : map (x+) (0 : map (y+) (0 : map (z+) ([0])))
--                       = 0 : map (x+) (0 : map (y+) (0 : [z]))
--                       = 0 : map (x+) (0 : map (y+) [0, z])
--                       = 0 : map (x+) (0 : [y+0, y+z])
--                       = 0 : map (x+) [0, y+0, y+z]
--                       = 0 : [x+0, x+y+0, x+y+z]
--                       = [0, x+0, x+y+0, x+y+z]
------         ∴  scanl @ e = foldr f [e]
------                          where
------                            f x xs = e : (x@) xs

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f (x:[]) = x
foldr1 f (x:xs) = f x (foldr1 f xs)
--- Write it as a Fusion Law
-- foldr1 # . foldr (\x xs -> e : map (x@) xs) e = foldr h b
--

test1 :: [Int] -> Int
test1 = maximum . scanl (+) 0

test2 :: [Int] -> Int
test2 = maximum . foldr f [0]
  where
    f x xs = 0 : map (x+) xs
