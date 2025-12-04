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
                 , age   :: Int }
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

hogofM'''' :: Int -> Maybe Int
hogofM''''  n =
  hM n >>= (\x ->
  gM x >>= (\y ->
  fM y))

hogofM''''Sugar :: Int -> Maybe Int
hogofM''''Sugar n = do
  x <- hM n
  y <- gM x
  fM y

-- λ> [1..5] >>= \x -> [x, -x]
-- [1,-1,2,-2,3,-3,4,-4,5,-5]

l1 :: [(Int, Char)]
l1 = [1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)

l2 :: [(Integer, Char)]
l2 =
  [1, 2] >>= (\n ->
  ['a', 'b'] >>= (\ch ->
  return (n, ch)))

l3 :: [(Integer, Char)]
l3 = do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)

l4 :: [(Int, Char)]
l4 = [ (n, ch) | n <- [1, 2], ch <- ['a', 'b'] ]

-- instance Monad Maybe where
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  -- Nothing >>= _ = Nothing
  -- Just x  >>= f = f x

{---------------------------------|
|--- Validation By Applicative ---|
|---------------------------------}
validations :: [Int -> Bool]
validations = [even, (>0)]

inputs :: [Int]
inputs = [4, -5]
-- λ> validations <*> inputs
-- [True,False,True,False]

-----

validateName :: String -> Either String String
validateName "" = Left "Name cannot be empty"
validateName n  = Right n

validateAge :: Int -> Either String Int
validateAge a
  | a < 0     = Left "Age cannot be negative"
  | otherwise = Right a

createUser :: String -> Int -> Either String User
createUser n a = User <$> validateName n <*> validateAge a
-- λ> createUser "John" 25
-- Right (User {name = "John", age = 25})
-- λ> createUser "John" (-4)
-- Left "Age cannot be negative"

-----

data Validation e a = Failure [e] | Success a
  deriving (Show, Eq)

instance Applicative (Validation e) where
  pure :: a -> Validation e a
  pure = Success

  (<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
  Failure e1 <*> Failure e2 = Failure (e1 ++ e2)
  Failure e1 <*> _          = Failure e1
  _          <*> Failure e2 = Failure e2
  Success f  <*> Success x  = Success (f x)

instance Functor (Validation e) where
  fmap :: (a -> b) -> Validation e a -> Validation e b
  fmap _ (Failure e) = Failure e
  fmap f (Success x) = Success (f x)

checkName :: String -> Validation String String
checkName "" = Failure ["Name cannot be empty"]
checkName n  = Success n

checkAge :: Int -> Validation String Int
checkAge a
  | a < 0     = Failure ["Age cannot be negative"]
  | otherwise = Success a

mkUser :: String -> Int -> Validation String User
mkUser name age = User <$> checkName name <*> checkAge age
-- λ> mkUser "John" 25
-- Success (User {name = "John", age = 25})
-- λ> mkUser "" (-5)
-- Failure ["Name empty!","Age negative!"]

handleUserCreationResult :: Validation String User -> IO ()
handleUserCreationResult validation =
  case validation of
    Success user ->
      putStrLn $ "Success:" ++ show user
    Failure es -> do
      putStrLn "Validation Failed:"
      mapM_ (putStrLn . (" - " ++) . show) es


{---------------------------|
|--- Self-created Option ---|
|---------------------------}
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
  fmap _ None     = None
  fmap f (Some x) = Some (f x)
