data FramedRose = EmptyRose
               | Rose { petal :: Petal
                      , innerRose :: FramedRose
                      }
                      deriving (Show, Eq)

data Petal = Petal { frameSquare :: Square
                   , innerCircle :: Circle
                   , innerSquare :: Square
                   }
                   deriving (Show, Eq)

newtype Square = Square { side :: Float } deriving (Show, Eq)
newtype Circle = Circle { radius :: Float } deriving (Show, Eq)

class HasArea a where
  area :: a -> Float

instance HasArea FramedRose where
  area :: FramedRose -> Float
  area rose
    | rose == EmptyRose = 0
    | otherwise         = area (petal rose) + area (innerRose rose)

instance HasArea Petal where
  area :: Petal -> Float
  area (Petal _ innerCircle innerSquare) = area innerCircle - area innerSquare

instance HasArea Square where
  area :: Square -> Float
  area (Square side) = sqr side

instance HasArea Circle where
  area :: Circle -> Float
  area (Circle radius) = pi * sqr radius

mkFramedRose :: Float -> FramedRose
mkFramedRose frameSide
  | isSmallEnough frameSide = EmptyRose
  | otherwise               = Rose petal innerRose
  where
    petal = mkPetal frameSide
    innerRose = mkFramedRose innerFrameSide
    innerFrameSide = hypotenuseOfRightTri $ half frameSide

mkPetal :: Float -> Petal
mkPetal frameSide = Petal frameSquare innerCircle innerSquare
  where
    frameSquare = Square frameSide
    innerCircle = Circle $ half frameSide
    innerSquare = Square (hypotenuseOfRightTri $ half frameSide)

hypotenuseOfRightTri :: Float -> Float
hypotenuseOfRightTri a = hypotenuse a a

hypotenuse :: Float -> Float -> Float
hypotenuse a b = sqrt (sqr a + sqr b)

isSmallEnough :: Float -> Bool
isSmallEnough n = n <= epsilon
  where
    epsilon = 0.0001

half :: Float -> Float
half = (/ 2)

sqr :: Float -> Float
sqr n = n * n
