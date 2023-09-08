module HigherOrderFunctions where

twice :: (a -> a) -> a -> a
twice f x = f (f x)

thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

twice' :: (a -> a) -> a -> a
twice' f x = (f.f) x

thrice' :: (a -> a) -> a -> a
thrice' f x = (f.f.f) x

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' predicate xs = [x | x <- xs, predicate x]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' mapper (x:xs) = mapper x : map' mapper xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' predicate (x:xs)
    | predicate x = x : filtered
    | otherwise = filtered
    where
        filtered = filter'' predicate xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ b []     = b
foldr' f b (x:xs) = f x (foldr' f b xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ b []     = b
foldl' f b (x:xs) = foldl' f (f b x) xs

-- foldl' (flip (:)) [] [1,2,3] = foldl' (flip (:)) ((flip (:)) [] 1) [2,3]
--                             = foldl' (flip (:)) [1] [2,3]
--                             = foldl' (flip (:)) ((flip (:)) [1] 2) [3]
--                             = foldl' (flip (:)) [2,1] [3]
--                             = foldl' (flip (:)) ((flip (:)) [2,1] 3) []
--                             = foldl' (flip (:)) [3,2,1] []
--                             = [3,2,1]

-- map' (*2) [1..5]
---- [2,4,6,8,10]

-- foldr' ((:) . (*2)) [] [1..5]
---- [2,4,6,8,10]

-- foldl' ((:) . (*2)) [] [1..5]
---- ERROR

-- :t flip
---- flip :: (a -> b -> c) -> b -> a -> c

-- foldl' (flip ((:) . (*2))) [] [1..5]
---- [10,8,6,4,2]
