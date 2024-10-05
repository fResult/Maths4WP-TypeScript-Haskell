import Data.Bifunctor

class Bifunctor' (p :: * -> * -> *) where
  bimap' :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap' f g = first' f . second' g

  first' :: (a -> b) -> p a c -> p b c
  first' f = bimap' f id

  second' :: (b -> c) -> p a b -> p a c
  second' = bimap' id
  {-# MINIMAL bimap' | first', second' #-}

instance Bifunctor' Either where
  bimap' :: (a -> b) -> (c -> d) -> Either a c -> Either b d
  bimap' fLeft _ (Left x) = Left (fLeft x)
  bimap' _ fRight (Right x) = Right (fRight x)

instance Bifunctor' (,) where
  bimap' :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
  bimap' f g (x, y) = (f x, g y)

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

main1 :: IO ()
main1 =
  let
    mapEither' :: Either [a] Int -> Either Int Bool
    mapEither' = bimap' length even
   in
    do
      putStrLn $ "mapEither even length (Left 42)          -- " ++ show (mapEither (Left 42))
      putStrLn $ "mapEither even length (Left 9)           -- " ++ show (mapEither (Left 9))
      putStrLn $ "mapEither even length (Right ['a'..'z']) -- " ++ show (mapEither (Right ['a' .. 'z']))
      putStrLn $ "mapEither even length (Right [0..14])    -- " ++ show (mapEither (Right [0 .. 14]))
      putStrLn "-------"
      putStrLn $ "mapEither' length even (Right 42)        -- " ++ show (mapEither' (Right 42))
      putStrLn $ "mapEither' length even (Left [0..15])    -- " ++ show (mapEither' (Left [0 .. 15]))

main2 :: IO ()
main2 =
  let
    mapTuple' :: ([a], Int) -> (Int, Bool)
    mapTuple' = bimap' length even
   in
    do
      putStrLn $ "mapTuple' length even (\"Hello\", 42) -- " ++ show (mapTuple' ("Hello", 42))
