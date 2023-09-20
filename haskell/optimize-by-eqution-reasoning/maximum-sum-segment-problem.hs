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
--                   = maximum . map (maximum . scanl (+) 0)) . tails                   <-- map (foldl (+) 0) . inits = scanl -- Naive Scanl, but we replace with the more efficient Scanl
