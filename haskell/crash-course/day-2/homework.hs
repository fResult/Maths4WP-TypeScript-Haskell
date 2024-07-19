newtype Circle = Circle {radius :: Float} deriving (Show, Eq)

newtype Square = Square {side :: Float} deriving (Show, Eq)

data Rectangle = Rectangle
  { height :: Float,
    weight :: Float
  }
  deriving (Show, Eq)

areaCircle :: Circle -> Float
areaCircle c = pi * r * r where r = radius c

areaRectangle :: Rectangle -> Float
areaRectangle rect = h * w
  where
    h = height rect
    w = weight rect

areaSquare :: Square -> Float
areaSquare square = areaRectangle Rectangle {height = s, weight = s}
  where
    s = side square

c10 = Circle {radius = 10}

sq10 = Square {side = 10}
