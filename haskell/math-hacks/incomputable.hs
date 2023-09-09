module Incomputable where

-- It is incomputable (non-terminate), the greatest hack ever in mathematical computing
to :: a -> Bool
to be = not (to be)
-- to "Something"
---- Run forever

-- head []
---- *** Exception: Prelude.head: empty list

-- 7 `div` 0
---- *** Exception: divide by zero

partial :: Bool -> Bool
partial True = True
-- partial False
---- *** Exception: Non-exhaustive patterns in function partial
