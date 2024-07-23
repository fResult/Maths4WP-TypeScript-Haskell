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

sqr :: Float -> Float
sqr x = x * x

hypotenuse :: Float -> Float -> Float
hypotenuse a b = sqrt $ sqr a + sqr b

hypotenuseEq :: Float -> Float
hypotenuseEq a = hypotenuse a a

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

-- Preparation function
areaInnerCircle :: Circle -> Float
areaInnerCircle circle = 0

-- Preparation function
areaInnerSquare :: Square -> Float
areaInnerSquare square = 0

areaOuterShape :: OuterShape -> Float
areaOuterShape shape = areaCircle circle - areaSquare square
 where
  circle = innerCircle shape
  square = innerSquare shape

isBaseShape :: Shape -> Bool
isBaseShape = isNothing . innerShape

isEpsilon :: Float -> Bool
isEpsilon x = x < epsilon
 where
  epsilon = 0.0000000001

mkOuterShape :: Float -> OuterShape
mkOuterShape frameSize =
  OuterShape
    { caseSquare = Square{side = frameSize}
    , innerCircle = Circle{radius = half frameSize}
    , innerSquare = Square{side = sqSide}
    }
 where
  sqSide = hypotenuseEq $ half frameSize

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

main :: IO ()
main = do
  let outerShp = mkOuterShape 100
  let area = areaOuterShape outerShp
  print outerShp
  putStrLn $ "The area is: " ++ show area
