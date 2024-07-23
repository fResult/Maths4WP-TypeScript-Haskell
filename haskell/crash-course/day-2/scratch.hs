import Data.Maybe (isNothing)

-- models
newtype Circle = Circle {radius :: Float}
  deriving (Show, Eq)

newtype Square = Square {side :: Float}
  deriving (Show, Eq)

data Rectangle = Rectangle {height :: Float, weight :: Float}
  deriving (Show, Eq)

data Shape = Shape
  { outerShape :: OuterShape
  , innerShape :: Maybe Shape
  }
  deriving (Show)

data OuterShape = OuterShape
  { caseSquare :: Square
  , innerCircle :: Circle
  , innerSquare :: Square
  }
  deriving (Show)

-- Utilities
half :: Float -> Float
half = (/ 2)

-- Functions
areaCircle :: Circle -> Float
areaCircle c = pi * r * r
 where
  r = radius c

areaRectangle :: Rectangle -> Float
areaRectangle rect = h * w
 where
  h = height rect
  w = weight rect

areaSquare :: Square -> Float
areaSquare square = areaRectangle Rectangle{height = s, weight = s}
 where
  s = side square

areaDonut :: Circle -> Circle -> Float
areaDonut c1 c2 = abs $ circleArea1 - circleArea2
 where
  circleArea1 = areaCircle c1

  circleArea2 = areaCircle c2

isBaseShape :: Shape -> Bool
isBaseShape = isNothing . innerShape

isEpsilon :: Float -> Bool
isEpsilon x = x < epsilon
  where epsilon = 0.0000000001

-- mkOuterShape :: Int -> OuterShape
-- mkOuterShape frameSize =
--   if isEpsilon

-- areaShape :: Shape -> Float
-- areaShape shape =
--   if isBaseShape shape
--     then 0
--     else 1 -- findMoreInnerShapeAreas

-- Test data
c5 = Circle{radius = 5}

c10 = Circle{radius = 10}

c20 = Circle{radius = 20}

sq10 = Square{side = 10}

sq20 = Square{side = 20}
