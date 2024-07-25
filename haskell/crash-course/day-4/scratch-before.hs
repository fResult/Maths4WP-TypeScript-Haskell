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
