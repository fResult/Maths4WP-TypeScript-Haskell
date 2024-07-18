{-# LANGUAGE GADTs, StandaloneDeriving #-}
data Circle
data Rectangle
data Square

data Shape a where
  Circle :: Float -> Shape Circle
  Rectangle :: Float -> Float -> Shape Rectangle
  Square :: Float -> Shape Rectangle

deriving instance Show (Shape a)
deriving instance Eq (Shape a)

area :: Shape a -> Float
area (Circle r) = pi * r * r
area (Rectangle h w) = h * w
area (Square s) = area (Rectangle s s)

radius :: Shape Circle -> Float
radius (Circle radius) = radius

perimeter :: Shape a -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle h w) = h * 2 + w * 2
perimeter (Square s) = perimeter (Rectangle s s)

-- area (Circle 10) -- 314.15927
-- area (Rectangle 20 10) -- 200
-- area (Square 10) -- 100

-- radius(Circle 10) -- 10

-- perimeter (Circle 10) -- 62.831856
-- perimeter (Rectangle 20 10) -- 60
-- perimeter (Square 10) -- 40