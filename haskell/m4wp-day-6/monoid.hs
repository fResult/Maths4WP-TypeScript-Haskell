{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}

newtype Sum a = Sum {getSum :: a} deriving (Show)

-- instance Num a => Semigroup (Sum a) where
--   mempty = Sum 0
--   mappend (Sum a) (Sum b) = Sum (a + b)

newtype Any = Any {getAny :: Bool} deriving (Show)
instance Semigroup Any where
  (<>) :: Any -> Any -> Any
  (<>) = mappend
instance Monoid Any where
  mempty :: Any
  mempty = Any False
  mappend :: Any -> Any -> Any
  mappend (Any x) (Any y) = Any (x || y)

newtype All = All {getAll :: Bool} deriving (Show)
instance Semigroup All where
  (<>) :: All -> All -> All
  (<>) = mappend
instance Monoid All where
  mempty :: All
  mempty = All True
  mappend :: All -> All -> All
  mappend (All x) (All y) = All (x && y)

test :: Maybe Integer
test = Just 1 >>= \x -> Just 2 >>= \y -> return (x + y)
