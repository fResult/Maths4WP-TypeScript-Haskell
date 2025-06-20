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

notDefined :: Bool -> Bool
notDefined = undefined
-- notDefined True
---- *** Exception: Prelude.undefined
------ CallStack (from HasCallStack):
-------- error, called in base:GHC.Err
-------- undefined, called in main:Incomputable

-- notDefined
---- <interactive>:24:1: error:
------ • No instance for (Show (Bool -> Bool))
------ • In a stmt of an interactive GHCi command: print it

notDefined2 :: Bool -> (Bool -> Bool)
notDefined2 = undefined
-- notDefined2 True
---- <interactive>:29:1: error:
------ • No instance for (Show (Bool -> Bool))
------ • In a stmt of an interactive GHCi command: print it

-- notDefined2 True True
---- *** Exception: Prelude.undefined
------ CallStack (from HasCallStack):
-------- error, called in base:GHC.Err
-------- undefined, called in main:Incomputable

or' :: Bool -> Bool -> Bool
-- or' _ undefined = error "Not support undefined"
or' True  _ = True
or' False b = b
-- or' True undefined
---- True <-- It should be error, so it's unexpected using

--- PROVE incomputable of fusion law ---
-- f (foldr g a xs) = foldr h b xs
----- CASE undefined
-- f (foldr g a undefined) = foldr h b undefined
--- WHEN we call `foldr g a undefined` ---
--- What is it evaluated to? ---
---- *** Exception: Prelude.undefined <-- It's still undefined
-- So... f (foldr g a undefined) = foldr h b undefined
--       f undefined             = undefined
