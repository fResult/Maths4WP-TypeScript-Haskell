import Data.Functor (fmap)
import Data.Maybe (fromJust)

add :: Int -> Int -> Int
add = (+)

mult :: Int -> Int -> Int
mult = (*)

double :: Int -> Int
double = (*2)

triple :: Int -> Int
triple = (*3)

quad :: Int -> Int
quad = (*4)

funs :: [Int -> Int]
funs = [double, triple, quad]
-- λ> funs <*> [2,3]
-- [4,6,6,9,8,12]

-- λ> funs <*> [10, 100, 1000]
-- [20,200,2000,30,300,3000,40,400,4000]
-- λ> fmap mult [2..4] <*> [10, 100, 1000]
-- [20,200,2000,30,300,3000,40,400,4000]

mfs :: Functor f => f Int -> f (Int -> Int)
mfs = fmap add
-- λ> :type mfs [2]
-- mfs [2] :: [Int -> Int]
-- λ> :type mfs $ Just 2
-- mfs $ Just 2 :: Maybe (Int -> Int)

-- λ> :type mfs [2]
-- mfs [2] :: [Int -> Int]
-- λ> mfs [2] <*> [10]
-- [12]

fmap' :: (a -> b) -> Maybe a -> Maybe b
fmap' f mx = Just (f (fromJust mx))

data User = User { name  :: String
                 , age   :: Int
                 , email :: String }
                 deriving (Show, Eq)

fM :: Int -> Maybe Int
fM n
  | n == 42   = Nothing -- For Arbitrary `Nothing` value
  | otherwise = Just n
-- λ> Just 40 >>= fM
-- Just 40
-- λ> Just 42 >>= fM
-- Nothing

gM :: Int -> Maybe Int
gM n
  | n == 58   = Nothing -- For Arbitrary `Nothing` value
  | otherwise = Just n

hM :: Int -> Maybe Int
hM n
  | n == 61   = Nothing -- For Arbitrary `Nothing` value
  | otherwise = Just n

f :: Int -> Int
f _ = 42

g :: Int -> Int
g _ = 58

h :: Int -> Int
h _ = 61

gofM :: Int -> Maybe Int
gofM n =
  case gM n of
    Nothing  -> Nothing
    (Just x) -> fM x

hogofM :: Int -> Maybe Int
hogofM n =
  case hM n of
    Nothing -> Nothing
    Just x  ->
      case gM x of
        Nothing -> Nothing
        Just x  -> fM x

hogofM' :: Int -> Maybe Int
hogofM' n =
  case hM n of
    Nothing -> Nothing
    Just x  -> gofM x

hogofM'' :: Int -> Maybe Int
hogofM'' n = fM n >>= gM >>= hM

hogofM''' :: Int -> Maybe Int
hogofM''' n = hM =<< gM =<< fM n

-- instance Monad Maybe where
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  -- Nothing >>= _ = Nothing
  -- Just x  >>= f = f x

data Option a = None | Some a
  deriving (Show, Eq)

fromSome :: Option a -> a
fromSome (Some x) = x

instance Monad Option where
  (>>=) :: Option a -> (a -> Option b) -> Option b
  None   >>= _ = None
  Some x >>= f = f x

instance Applicative Option where
  pure :: a -> Option a
  pure = Some

  (<*>) :: Option (a -> b) -> Option a -> Option b
  None   <*> _      = None
  _      <*> None   = None
  Some f <*> Some x = Some $ f x

instance Functor Option where
  fmap :: (a -> b) -> Option a -> Option b
  fmap = undefined
