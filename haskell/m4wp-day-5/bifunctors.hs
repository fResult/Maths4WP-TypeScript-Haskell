import Data.Bifunctor

{-- Doing Algebra --}
-- even   :: Int -> Bool
-- length :: [a] -> Int

-- bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
----     :: (Int -> Bool) -> ([a] -> Int) -> p Int [a] -> p Bool Int

-- So...
-- bimap even length :: Bifunctor p => p Int [a] -> p Bool Int

-- Test
mapEither :: (Bifunctor p) => p Int [a] -> p Bool Int
mapEither = bimap even length

main :: IO ()
main = do
  putStrLn $ "mapEither even length (Left 42)          -- " ++ show (mapEither (Left 42))
  putStrLn $ "mapEither even length (Left 9)           -- " ++ show (mapEither (Left 9))
  putStrLn $ "mapEither even length (Right ['a'..'z']) -- " ++ show (mapEither (Right ['a' .. 'z']))
  putStrLn $ "mapEither even length (Right [0..14])    -- " ++ show (mapEither (Right [0 .. 14]))
