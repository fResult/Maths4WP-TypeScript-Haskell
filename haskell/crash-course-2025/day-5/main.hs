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
    petal = undefined
    innerRose = undefined

isSmallEnough :: Float -> Bool
isSmallEnough x = undefined
