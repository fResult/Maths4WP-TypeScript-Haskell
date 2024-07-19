{-# LANGUAGE ExistentialQuantification #-}
import Text.Read (Lexeme(String))
-- {-# LANGUAGE ExplicitForAll #-}
-- {-# LANGUAGE StandaloneDeriving #-}

-- instance Eq Shape where
--   (==) :: Shape -> Shape -> Bool
--   a == b = show a == show b

-- instance Ord Shape where
--   Shape a <= Shape b = (area a) <= (area b)

newtype Circle = Circle Float deriving (Show, Eq)

data Rectangle = Rectangle Float Float deriving (Show, Eq)

newtype Square = Square Float deriving (Show, Eq)

class HasArea a where
  area :: a -> Float

instance HasArea Circle where
  area (Circle r) = pi * r * r

instance HasArea Rectangle where
  area (Rectangle h w) = h * w

instance HasArea Square where
  area (Square s) = area (Rectangle s s)

data Shape = forall a. (Show a, Eq a, HasArea a) => Shape a

-- Remove the invalid deriving instance statement

-- data Circle
-- data Rectangle
-- data Square

-- data Shape a where
--   Circle :: Float -> Shape Circle
--   Rectangle :: Float -> Float -> Shape Rectangle
--   Square :: Float -> Shape Rectangle

-- deriving instance Show (Shape a)
-- deriving instance Eq (Shape a)

-- area :: Shape a -> Float
-- area (Circle r) = pi * r * r
-- area (Rectangle h w) = h * w
-- area (Square s) = area (Rectangle s s)

-- radius :: Shape Circle -> Float
-- radius (Circle radius) = radius

-- perimeter :: Shape a -> Float
-- perimeter (Circle r) = 2 * pi * r
-- perimeter (Rectangle h w) = h * 2 + w * 2
-- perimeter (Square s) = perimeter (Rectangle s s)

-- area (Circle 10) -- 314.15927
-- area (Rectangle 20 10) -- 200
-- area (Square 10) -- 100

-- radius(Circle 10) -- 10

-- perimeter (Circle 10) -- 62.831856
-- perimeter (Rectangle 20 10) -- 60
-- perimeter (Square 10) -- 40

-- newtype Circle = Circle Float deriving (Show, Eq)
-- data Rectangle = Rectangle Float Float deriving (Show, Eq)
-- newtype Square = Square Float deriving (Show, Eq)

-- data Shape
--   = ShapeCircle Circle
--   | ShapeRectangle Rectangle
--   | ShapeSquare Square
--   | ShapeRectangle2 Rectangle
--   deriving (Show, Eq)

-- area :: Shape -> Float
-- area (ShapeCircle (Circle r)) = pi * r * r
-- area (ShapeRectangle (Rectangle l w)) = l * w
-- area (ShapeSquare (Square s)) = area (ShapeRectangle (Rectangle s s))

-- area $ ShapeCircle $ Circle 10
-- area $ ShapeRectangle $ Rectangle 10 20
-- area $ ShapeSquare $ Square 10
-- data Shape a where
--     Circle :: Float -> Shape Circle
--     Square :: Float -> Shape Square
--     Rectangle :: Float -> Float -> Shape Rectangle

-- deriving instance Show (Shape a)
-- deriving instance Eq (Shape a)

-- class Shape a where
--     area :: a -> Circle
--     perimeter :: a -> Circle

-- instance Shape Circle where
--     area (Circle r) = pi * r * r
--     perimeter (Circle r) = 2 * pi * r

newtype BaseURL = BaseURL String deriving Show
newtype APIPath = APIPath String deriving Show

data APIEndpoint = APIEndpoint BaseURL APIPath
  deriving (Show)

