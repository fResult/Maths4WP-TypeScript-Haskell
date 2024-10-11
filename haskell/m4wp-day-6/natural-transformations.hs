{-# LANGUAGE AllowAmbiguousTypes #-}
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

main1 :: IO ()
main1 = do
  putStrLn $ "fmap even Nothing: " ++ show (fmap even Nothing)
  putStrLn $ "fmap even (Just 10): " ++ show (fmap even (Just 10))
  print "====================="
  putStrLn $ "fmap even []: " ++ show (fmap even [])
  putStrLn $ "fmap even [10]: " ++ show (fmap even [10])
  print "========= Commutative =========="
  putStrLn $ "(fmap even . maybeToList) Nothing: " ++ show (fmap even . maybeToList $ Nothing)
  putStrLn $ "(maybeToList . fmap even) Nothing: " ++ show (maybeToList . fmap even $ Nothing)
  putStrLn $ "(fmap even . maybeToList) (Just 10): " ++ show (fmap even . maybeToList $ Just 10)
  putStrLn $ "(maybeToList . fmap even) (Just 10): " ++ show (maybeToList . fmap even $ Just 10)

f :: Int -> Bool
f = undefined
-- f = even
-- f = odd
-- f = (> 10)
-- f = isPrime

natural :: (Functor f, Functor g) => f a -> g a
natural = undefined
-- natural = reverse
-- natural = tail
-- natural = seqList
-- natural = cycle

-- n1 :: [Int] -> [Bool]
-- n1 = fmap f . natural
-- n2 :: [Int] -> [Bool]
-- n2 = natural . fmap f
-- -- n1 and n2 are commutative

ints2Ints :: [Int] -> [Int]
ints2Ints = undefined

bools2Bools :: [Bool] -> [Bool]
bools2Bools = undefined

natural' :: (Functor f, Functor g) => f Int -> g Int
natural' = undefined

nX :: (Functor f, Functor g) => f Int -> f Bool
nX = fmap f . natural

nY :: (Functor f, Functor g) => f Int -> g Bool
nY = natural . fmap f

f' :: Int -> Int
f' = undefined

nX' :: (Functor f, Functor g) => f Int -> f Int
nX' = fmap f' . natural

nY' :: (Functor f, Functor g) => f Int -> g Int
nY' = natural . fmap f'

nX'' :: (Functor f, Functor g) => f Int -> f Int
nX'' = fmap f' . natural'

nY'' :: (Functor f, Functor g) => f Int -> g Int
nY'' = natural' . fmap f'
