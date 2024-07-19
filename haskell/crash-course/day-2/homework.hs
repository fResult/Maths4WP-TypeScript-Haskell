newtype Circle = Circle {radius :: Float} deriving (Show, Eq)

newtype Square = Square {side :: Float} deriving (Show, Eq)

data Rectangle = Rectangle
  { height :: Float,
    weight :: Float
  }
  deriving (Show, Eq)

-- newtype Circle = Circle Float deriving Show

areaCircle :: Circle -> Float
areaCircle c = pi * r * r where r = radius c

areaRectangle :: Rectangle -> Float
areaRectangle rect = h * w
  where
    h = height rect
    w = weight rect

c10 = Circle {radius = 10}

areaTest :: Int -> Int
areaTest a = a
