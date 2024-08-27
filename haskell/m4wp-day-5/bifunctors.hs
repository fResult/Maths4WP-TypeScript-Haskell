-- even   :: Int -> Bool
-- length :: [a] -> Int

-- bimap  :: (a -> b) -> (c -> d) -> p a c -> p b d
---- (Int -> Bool) -> ([a] -> Int) -> p Int [a] -> p Bool Int

-- bimap even length :: Bifunctor p => p Int [a] -> p Bool Int

-- Test
-- bimap even length (Left 42)          -- True
-- bimap even length (Left 9)           -- False
-- bimap even length (Right ['a'..'z']) -- 26
-- bimap even length (Right [0..14])    -- 15
