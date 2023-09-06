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
-- foldr' (+) 0 [1..10] = 55

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ b []     = b
foldl' f b (x:xs) = foldl' f (f b x) xs
