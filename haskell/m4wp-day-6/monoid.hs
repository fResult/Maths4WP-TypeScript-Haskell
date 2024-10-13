import Data.List
import Data.Char

newtype Sum a = Sum {getSum :: a} deriving (Show)

-- instance Num a => Semigroup (Sum a) where
--   mempty = Sum 0
--   mappend (Sum a) (Sum b) = Sum (a + b)

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

testBinding :: Maybe Int
testBinding = Just 1 >>= \x -> Just 2 >>= \y -> return (x + y)
