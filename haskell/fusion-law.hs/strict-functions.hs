module StrictFunctions where

--- Prove Strict Functions ---
---- foldr f' a' . map g'                     <-- 1
--            (f . foldr g a) = f h b         <-- 2
--            (f . foldr ((:) . g') [])

---- map g' = foldr ((:) . g') []             <-- 1
---- g = ((:) . g'), a = []                   <-- 2
---- f = foldr f' a'                          <-- 1, 2

double :: Int -> Int
double = (*2)
-- double . sum $ [1..10]
---- 110
-- sum . map double $ [1..10]
---- 110
-- foldr (+) 0 . map double $ [1..10]
---- 110
-- foldr ((+) . double) 0 $ [1..10]
---- 110

-- foldr f a . map g = foldr (f . g) a
