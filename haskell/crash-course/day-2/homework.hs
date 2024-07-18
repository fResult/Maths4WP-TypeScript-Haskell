newtype Circle = Circle Float deriving (Show, Eq)

data Rectangle = Rectangle Float Float deriving (Show, Eq)

newtype Square = Square Float deriving (Show, Eq)

data Shape
  = ShapeCircle Circle
  | ShapeRectangle Rectangle
  | ShapeSquare Square
  | ShapeRectangle2 Rectangle
  deriving (Show, Eq)

area :: Shape -> Float
area (ShapeCircle (Circle r)) = pi * r * r
area (ShapeRectangle (Rectangle l w)) = l * w
area (ShapeSquare (Square s)) = area (ShapeRectangle (Rectangle s s))
