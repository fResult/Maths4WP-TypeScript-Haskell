module StrictFunctions where

--- Prove Strict Functions ---
---- foldr f' a' . map g'                     <-- 1
--            (f . foldr g a) = f h b         <-- 2
--            (f . foldr ((:) . g') [])

---- map g' = foldr ((:) . g') []             <-- 1
---- g = ((:) . g'), a = []                   <-- 2
---- f = foldr f' a'                          <-- 1, 2
