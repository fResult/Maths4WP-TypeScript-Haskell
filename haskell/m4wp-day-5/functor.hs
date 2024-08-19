import Text.PrettyPrint.Annotated.HughesPJClass (Pretty (pPrintList))

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
--   {-# MINIMAL fmap #-}
--         -- Defined in ‘GHC.Base’
-- instance Functor (Either a) -- Defined in ‘Data.Either’
-- instance Functor [] -- Defined in ‘GHC.Base’
-- instance Functor Maybe -- Defined in ‘GHC.Base’
-- instance Functor IO -- Defined in ‘GHC.Base’
-- instance Functor ((->) r) -- Defined in ‘GHC.Base’
-- instance Functor ((,) a) -- Defined in ‘GHC.Base’
-- instance Functor ((,,) a b) -- Defined in ‘GHC.Base’
-- instance Functor ((,,,) a b c) -- Defined in ‘GHC.Base’

main1 :: IO ()
main1 = do
  print $ even 5 -- False
  print $ even 8 -- True
  ---
  print $ length ("word" :: String) -- 4
  print $ length ("print" :: String) -- 5
  print $ (even . length) ("string" :: String) -- True
  ---
  print $ fmap even [1 .. 10]
  print $ fmap length (["one", "two", "three", "four", "five"] :: [String])

{--# Some Instances of Functor #--}
-- data [] a = [] | a:[]
-- instance Functor [] where
--   fmap = map
-- `[] a` is a Type Constructor

data Optional a = None | Some a deriving (Show, Eq, Ord)
instance Functor' Optional where
  fmap' :: (a -> b) -> Optional a -> Optional b
  fmap' f None = None
  fmap' f (Some x) = Some (f x)
-- `Optional a` is also a Type Constructor

class Functor' (f :: * -> *) where
  fmap' :: (a -> b) -> f a -> f b

  -- (<<$) :: a -> f b -> f a
  {-# MINIMAL fmap' #-}

main2 :: IO ()
main2 = do
  print $ fmap' (even . length) $ Some ("Korn" :: String)
  print $ fmap' even . fmap' length $ Some ("fResult" :: [Char])

main3 :: IO ()
main3 =
  let name = Just "fResult"
      noName = Nothing
      mLength :: String -> Maybe Int
      mLength x = Just (length x)
      mEven :: Int -> Maybe Bool
      mEven x = Just (even x)

      mLengthEven :: String -> Maybe Bool
      mLengthEven x = Just $ (even . length) x

      nameLength = name >>= mLength
   in do
        putStrLn "Hello World!"
        putStrLn $ show name ++ " becomes " ++ show (name >>= mLength)
        putStrLn $ show nameLength ++ " becomes " ++ show (nameLength >>= mEven)
        putStrLn $ "Composed " ++ show name ++ " becomes " ++ show (name >>= mLength >>= mEven)
        putStrLn $ "More composed " ++ show name ++ " becomes " ++ show (name >>= mLengthEven)
        print (noName >>= mLength >>= mEven)
