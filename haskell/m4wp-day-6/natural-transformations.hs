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
