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
--               = [] : map (1:) ([] : map (2:) (inits' [3]))
--               = [] : map (1:) ([] : map (2:) ([] : map (3:) (inits' [])))
--               = [] : map (1:) ([] : map (2:) ([] : map (3:) [[]]))
--               = [] : map (1:) ([] : map (2:) ([] : [[3]]))
--               = [] : map (1:) ([] : map (2:) [[], [3]])
--               = [] : map (1:) ([] : [[2], [2, 3]])
--               = [] : map (1:) [[], [2], [2, 3]]
--               = [] : [[1], [1, 2], [1, 2, 3]]
--               = [[], [1], [1, 2], [1, 2, 3]]

maximumSumSegment :: [Int] -> Int
maximumSumSegment = maximum . map sum . segments'
