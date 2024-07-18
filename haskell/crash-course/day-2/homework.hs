newtype Circle = Circle Float

data Rectangle = Rectangle Float Float

newtype Square = Square Float

data Shape
  = ShapeCircle Circle
  | ShapeRectangle Rectangle
  | ShapeSquare Square
  | ShapeRectangle2 Rectangle

area :: Shape -> Float
area (ShapeCircle (Circle r)) = pi * r * r
area (ShapeRectangle (Rectangle l w)) = l * w
area (ShapeSquare (Square s)) = area (ShapeRectangle (Rectangle s s))

-- area $ ShapeCircle $ Circle 10
-- area $ ShapeRectangle $ Rectangle 10 20
-- area $ ShapeSquare $ Square 10
