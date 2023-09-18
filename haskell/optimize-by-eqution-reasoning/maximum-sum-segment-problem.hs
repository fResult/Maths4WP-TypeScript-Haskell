module MaximumSumSegmentProblem where
import Data.List as L (nub)

tails :: [a] -> [[a]]
tails []       = [[]]
tails s@(x:xs) = s : tails xs

inits :: [a] -> [[a]]
inits []     = [[]]
inits (x:xs) = [] : map (x:) (inits xs)

segments :: Eq a => [a] -> [[a]]
segments = nub . concat . map inits . tails
