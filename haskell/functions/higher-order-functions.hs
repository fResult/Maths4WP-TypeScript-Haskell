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


map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
-- map (*2) [1, 2, 3, 4] = (*2) 1 : map (*2) [2, 3, 4]
--                       = (*2) 1 : (*2) 2 : map (*2) [3, 4]
--                       = (*2) 1 : (*2) 2 : (*2) 3 : map (*2) [4]
--                       = (*2) 1 : (*2) 2 : (*2) 3 : (*2) 4 : map (*2) []
--                       = (*2) 1 : (*2) 2 : (*2) 3 : (*2) 4 : []
--                       = 1 : 4 : 6 : 8 : []
--                       = 1 : 4 : 6 : [8]
--                       = 1 : 4 : [6, 8]
--                       = 1 : [4, 6, 8]
--                       = [1, 4, 6, 8]
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs
-- sum [1, 2, 3] = 1 + sum [2, 3]
--               = 1 + 2 + sum [3]
--               = 1 + 2 + 3 + sum []
--               = 1 + 2 + 3 + 0
--               = 1 + 2 + 3
--               = 1 + 5
--               = 6

--               = ((+) 1 : ((+) 2 : ((+) 3 : 0)))