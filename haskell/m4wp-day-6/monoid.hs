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
  (<>) (Product x) (Product y) = Product (x + y)
instance Monoid Product where
  mempty :: Product
  mempty = Product 1
  mappend :: Product -> Product -> Product
  mappend = (<>)

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

main1 :: IO ()
main1 = do
  putStrLn $ "'MyPass' is forbidden?: " ++ show (isForbidden "MyPass")
  putStrLn $ "'MyPass' is allowed?: " ++ show (isAllowed "MyPass")
  putStrLn $ "'MyPass🌍' is forbidden?: " ++ show (isForbidden "MyPass🌍")
  putStrLn $ "'MyPass🌍' is allowed?: " ++ show (isAllowed "MyPass🌍")
  putStrLn $ "'MyPass1' is forbidden?: " ++ show (isForbidden "MyPass1")
  putStrLn $ "'MyPass1' is allowed?: " ++ show (isAllowed "MyPass1")

charPreds :: [Char -> Bool]
charPreds = [isAlpha, isUpper]
