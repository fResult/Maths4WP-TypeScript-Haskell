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
    petal = Petal (Square frameSide) (Circle $ half frameSide) (Square hypotenuse)
    innerRose = undefined
    hypotenuse = undefined

isSmallEnough :: Float -> Bool
isSmallEnough n = n <= epsilon
  where
    epsilon = 0.0001

half :: Float -> Float
half = (/ 2)
