import GHC.Stack (popCallStack)
sqr :: Float -> Float
sqr n = n * n

half :: Float -> Float
half = (/ 2)

hypotenuse :: Float -> Float -> Float
hypotenuse a b = sqrt $ sqr a + sqr b

hypotenuseEq :: Float -> Float
hypotenuseEq a = hypotenuse a a

isSmallEnough :: Float -> Bool
isSmallEnough n = n < epsilon
 where
  epsilon = 0.001

newtype Circle = Circle {_circleRadius :: Float} deriving (Show, Eq)
newtype Square = Square {_squareSide :: Float} deriving (Show, Eq)
data Rectangle = Rectangle {_rectWeight :: Float, _reactHeight :: Float}
  deriving (Show, Eq)

class HasArea a where
  area :: a -> Float

instance HasArea Circle where
  area :: Circle -> Float
  area c = pi * sqr (_circleRadius c)

instance HasArea Rectangle where
  area :: Rectangle -> Float
  area rect = _rectWeight rect * _reactHeight rect

instance HasArea Square where
  area :: Square -> Float
  area sq = area $ Rectangle s s
   where
    s = _squareSide sq

instance HasArea Petal where
  area :: Petal -> Float
  area petal = area (innerCircle petal) - area (innerSquare petal)

data Petal = Petal
  { frameSquare :: Square
  , innerCircle :: Circle
  , innerSquare :: Square
  }
  deriving (Show, Eq)

mkPetal :: Float -> Petal
mkPetal frameSize = Petal frameSquare innerCircle innerSquare
 where
  frameSquare = Square frameSize
  innerCircle = Circle $ half frameSize
  innerSquare = Square $ hypotenuseEq $ half frameSize

data FramedFlower
  = EmptyFlower
  | Flower {petal :: Petal, innerFlower :: FramedFlower}
  deriving (Show, Eq)

mkFramedFlower :: Float -> FramedFlower
mkFramedFlower frameSize
  | isSmallEnough frameSize = EmptyFlower
  | otherwise = Flower petal innerFlower
 where
  petal = mkPetal frameSize
  innerFlower = mkFramedFlower innerFrameSize
  innerFrameSize = hypotenuseEq (half frameSize)

instance HasArea FramedFlower where
  area :: FramedFlower -> Float
  area flower
    | flower == EmptyFlower = 0
    | otherwise = area (petal flower) + area (innerFlower flower)

data Shape = forall a. (Show a, Eq a, HasArea a) => Shape {shape :: a}
deriving instance Show Shape

instance HasArea Shape where
  area :: Shape -> Float
  area (Shape a) = area a

instance Eq Shape where
  (==) :: Shape -> Shape -> Bool
  a == b = show a == show b

instance Ord Shape where
  (<=) :: Shape -> Shape -> Bool
  a <= b = area a <= area b

s1 = Circle 10
s2 = Square 10
s3 = Shape $ mkPetal 10
s4 = Shape $ mkFramedFlower 10

-- Optional re-invent
data Optional a = Some a | None
  deriving (Show, Eq)

f :: Int -> Int
f _ = 7

g :: Int -> Int
g _ = 9

h :: Int -> Int
h _ = 17

f' :: Int -> Optional Int
f' n
  | n == 17 = None
  | otherwise = Some n

g' :: Int -> Optional Int
g' n
  | even n = None
  | otherwise = Some n

h' :: Int -> Optional Int
h' n
  | even n = Some n
  | otherwise = None

fog' :: Int -> Optional Int
fog' n =
  case g' n of
    None -> None
    Some x -> f' x

fogoh' :: Int -> Optional Int
fogoh' n =
  case h' n of
    None -> None
    Some x -> case g' x of
      None -> None
      Some y -> f' y

instance Functor Optional where
  fmap :: (a -> b) -> Optional a -> Optional b
  fmap _ None = None
  fmap f (Some x) = Some $ f x

instance Applicative Optional where
  pure :: a -> Optional a
  pure = Some

  (<*>) :: Optional (a -> b) -> Optional a -> Optional b
  _ <*> None = None
  None <*> (Some x) = None
  (Some f) <*> (Some x) = Some (f x)

instance Monad Optional where
  (>>=) :: Optional a -> (a -> Optional b) -> Optional b
  -- None >>= _ = None
  -- (Some x) >>= f = f x
  x >>= f = case x of
    None -> None
    Some x -> f x

fogohM :: Int -> Optional Int
fogohM n = h' n >>= g' >>= f'

-- fogohM n = do
--   x <- h' n
--   y <- g' x
--   f' y

fogohM' :: Int -> Optional Int
fogohM' n =
  h' n >>= (\y -> g' y >>= (\z -> f' z))

fogohM'' :: Int -> Optional Int
fogohM'' n = do
  y <- h' n
  z <- g' y
  return $ f' z


type Stack = [Int]

empty :: Stack
empty = []

pop :: Stack -> (Int, Stack)
pop [] = error "empty stack"
pop (x:xs) = (x, xs)

push :: Int -> Stack -> (Int, Stack)
push a xs = (a, a:xs)

pushS :: Int -> Stack -> Stack
pushS a xs = a:xs

-- stackManipulation :: Stack Int
-- stackManipulation s0 = do
  -- pushS 3
  -- a <- popS
  -- pushS 2
  -- b <- popS
  -- pushS 10
  -- pushS (a+b)
  -- s <- popS
  -- return s
