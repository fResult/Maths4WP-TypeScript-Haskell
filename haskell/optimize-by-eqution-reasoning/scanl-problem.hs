module ScanlProblem where

inits :: [a] -> [[a]]
inits []     = [[]]
inits (x:xs) = [] : map (x:) (inits xs)
-- inits [1,2,3] = [] : map (1:) (inits [2, 3])
--               = [] : map (1:) ([] : map (2:) (inits [3]))
--               = [] : map (1:) ([] : map (2:) ([] : map (3:) (inits [])))
--               = [] : map (1:) ([] : map (2:) ([] : map (3:) [[]]))
--               = [] : map (1:) ([] : map (2:) ([] : [[3]]))
--               = [] : map (1:) ([] : map (2:) [[], [3]])
--               = [] : map (1:) ([] : [[2], [2, 3]])
--               = [] : map (1:) ([[], [2], [2, 3]])
--               = [] : [[1], [1, 2], [1, 2, 3]]
--               = [[], [1], [1, 2], [1, 2, 3]]

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f e = (map (foldl f e)) . inits
-- scanl' (+) 0 [1,2,3] = map (foldl (+) 0) [[], [1], [1,2], [1,2,3]]
--                      = [0, (0+1), (0+1+2), (0+1+2+3)]
--                      = [0,1,3,6]

--- DO Equational Reasoning of Scanl ---
---- CASE [], let's assume...
---- scanl f e [] = (map (foldl f e) . inits) []
----              = map (foldl f e) (inits [])             <-- (g . f) x = g (f x)
----              = map (foldl f e) [[]]                   <-- inits [] = [[]]
----              = (foldl f e) [] : map (foldl f e) []    <-- map f (x:xs) = f x : map f xs
----              = (foldl f e) [] : []                    <-- map f []     = []
----              = [foldl f e []]                         <-- x : [] = [x]
----              = [e]                                    <-- foldl f b [] = b
------      ∴ scanl f e [] = [e]      ------
---- CASE (x:xs), let's assume...
---- scanl f e xs     = map (foldl f e) . (inits xs)
---- scanl f e (x:xs) = map (foldl f e) . (inits (x:xs))
---- scanl f e (x:xs) = map (foldl f e) . ([] : map (x:) (inits xs))       <-- inits (x:xs) = [] : map (x:) (inits xs)
