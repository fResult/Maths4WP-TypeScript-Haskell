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

mkFramedRose :: Float -> FramedRose
mkFramedRose frameSide
  | isSmallEnough frameSide = EmptyRose
  | otherwise               = Rose petal innerRose
  where
    petal = mkPetal frameSide
    innerRose = mkInnerRose innerFrameSide
    innerFrameSide = undefined

mkPetal :: Float -> Petal
mkPetal frameSide = Petal
  (Square frameSide)
  (Circle $ half frameSide)
  (Square $ hypotenuseEqual (half frameSide))

mkInnerRose :: Float -> FramedRose
mkInnerRose frameSide = undefined

hypotenuseEqual :: Float -> Float
hypotenuseEqual a = hypotenuse a a

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
