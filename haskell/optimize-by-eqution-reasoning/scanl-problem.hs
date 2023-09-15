module ScanlProblem where

inits :: [a] -> [[a]]
inits []     = [[]]
inits (x:xs) = [] : map (x:) (inits xs)
-- inits [1..10]
---- [[], [1], [1,2], [1,2,3], [1,2,3,4], [1,2,3,4,5], [1,2,3,4,5,6], [1,2,3,4,5,6,7], [1,2,3,4,5,6,8], [1,2,3,4,5,6,8,9], [1,2,3,4,5,6,8,9,10]]


scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f e = (map (foldl f e)) . inits
