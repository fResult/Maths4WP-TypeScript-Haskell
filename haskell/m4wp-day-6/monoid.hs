import Data.Char
import Data.List

newtype Sum = Sum {getSum :: Int} deriving (Show)
instance Semigroup Sum where
  (<>) :: Sum -> Sum -> Sum
  (<>) (Sum x) (Sum y) = Sum (x + y)
instance Monoid Sum where
  mempty :: Sum
  mempty = Sum 0
  mappend :: Sum -> Sum -> Sum
  mappend = (<>)

newtype Product = Product {getProduct :: Int} deriving (Show)
instance Semigroup Product where
  (<>) :: Product -> Product -> Product
  (<>) (Product x) (Product y) = Product (x * y)
instance Monoid Product where
  mempty :: Product
  mempty = Product 1
  mappend :: Product -> Product -> Product
  mappend = (<>)

main1 :: IO ()
main1 = do
  putStrLn $ "Sum 1 `mappend` Sum 2: " ++ show (getSum $ mappend (Sum 2) (Sum 3))
  putStrLn $ "mconcat $ map Sum [1..5]: " ++ show (getSum . mconcat $ map Sum [1..5])
  putStrLn $ "Product 1 `mappend` Product 2: " ++ show (getProduct $ Product 2 `mappend` Product 3)
  putStrLn $ "mconcat $ map Product [1..5]: " ++ show (getProduct . mconcat $ map Product [1..5])


newtype Any = Any {getAny :: Bool} deriving (Show)
instance Semigroup Any where
  (<>) :: Any -> Any -> Any
  (<>) (Any x) (Any y) = Any (x || y)
instance Monoid Any where
  mempty :: Any
  mempty = Any False
  mappend :: Any -> Any -> Any
  mappend = (<>)

newtype All = All {getAll :: Bool} deriving (Show)
instance Semigroup All where
  (<>) :: All -> All -> All
  (<>) (All x) (All y) = All (x && y)
instance Monoid All where
  mempty :: All
  mempty = All True
  mappend :: All -> All -> All
  mappend = (<>)

passAnyOf :: [a -> Bool] -> a -> Bool
passAnyOf preds x = getAny $ foldMap (Any .) preds x

-- ideal should be `passAnyOf = getAny . foldMap (Any .)

passAllOf :: [a -> Bool] -> a -> Bool
passAllOf preds x = getAll $ foldMap (All .) preds x

-- ideal should be `passAllOf = getAny . foldMap (Any .)

isSpecialCharacter :: Char -> Bool
isSpecialCharacter c = not $ isAlpha c

isForbidden :: String -> Bool
isForbidden = passAnyOf [any isSpace, any isSpecialCharacter]

isAllowed :: String -> Bool
isAllowed = passAllOf [any isAlpha, any isDigit]

main2 :: IO ()
main2 = do
  putStrLn $ "'MyPass' is forbidden?: " ++ show (isForbidden "MyPass")
  putStrLn $ "'MyPass' is allowed?: " ++ show (isAllowed "MyPass")
  putStrLn $ "'MyPass🌍' is forbidden?: " ++ show (isForbidden "MyPass🌍")
  putStrLn $ "'MyPass🌍' is allowed?: " ++ show (isAllowed "MyPass🌍")
  putStrLn $ "'MyPass1' is forbidden?: " ++ show (isForbidden "MyPass1")
  putStrLn $ "'MyPass1' is allowed?: " ++ show (isAllowed "MyPass1")

pass :: (Monoid c) => (b -> c) -> [a -> b] -> a -> c
pass mcb = mconcat . map (mcb .)

passAll' :: [a -> Bool] -> a -> All
passAll' = pass All

passAny' :: [a -> Bool] -> a -> Any
passAny' = pass Any

passAllOf' :: [a -> Bool] -> a -> Bool
passAllOf' preds = getAll . passAll' preds

passAnyOf' :: [a -> Bool] -> a -> Bool
passAnyOf' preds = getAny . passAny' preds

isForbidden' :: String -> Bool
isForbidden' = passAnyOf' [any isSpace, any isSpecialCharacter]

isAllowed' :: String -> Bool
isAllowed' = passAllOf' [any isAlpha, any isUpper]

main3 :: IO ()
main3 = do
  putStrLn $ "'Korn' is alpha and upper: " ++ show (getAll $ (mconcat . map (All .) $ [any isAlpha, any isUpper]) ("Korn"::String))
  putStrLn $ "'Korn' is forbidden?: " ++ show (isForbidden' "Korn")
  putStrLn $ "'Korn' is allowed?: " ++ show (isAllowed' "Korn")
  putStrLn $ "'Korn🌍' is forbidden?: " ++ show (isForbidden "Korn🌍")
  putStrLn $ "'Korn🌍' is allowed?: " ++ show (isAllowed "Korn🌍")

charPreds :: [String -> Bool]
charPreds = [any isAlpha, any isUpper]

sumNumbers :: [Int] -> Int
sumNumbers = getSum . mconcat . map Sum



