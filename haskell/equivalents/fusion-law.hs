double :: Int -> Int
double = (*2)

sumDouble :: [Int] -> Int
sumDouble = (double . sum)
-- sumDouble [1..10]
---- 110

sumDouble' :: [Int] -> Int
sumDouble' = foldr ((+) . double) 0
-- sumDouble' [1..10]
---- 110

lengthConcat :: [[a]] -> Int
lengthConcat = length . concat
-- lengthConcat [[1..10], [16..20]]
---- 15

lengthConcat' :: [[a]] -> Int
lengthConcat' = foldr ((+) . length) 0
-- lengthConcat' [[1..10], [16..20]]
---- 15

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' []         = b
foldr' f b (x:xs) = f x (foldr' f b xs)

--- PROVE FUSION LAW ---
-- f . (foldr g a)      = foldr h b
-- (f . (foldr g a)) xs = foldr h b xs
-- f (foldr g a xs)     = foldr h b xs
---- Induction on xs
----- CASE []
-- f (foldr g a []) = foldr h b []
-- f  a             = b
----- CASE (x:xs)
-- f (foldr g a (x:xs))   = foldr h b (x:xs)
-- f (g x (foldr g a xs)) = h x (foldr h b xs)
----- FOR foldr h b xs = f (foldr g a xs) -----
-- ∴   h x (foldr h b xs) = h x (f (foldr g a xs))
----- LET name `foldr g a xs` to be y -----
-- f (g x y)              = h x (f y)
