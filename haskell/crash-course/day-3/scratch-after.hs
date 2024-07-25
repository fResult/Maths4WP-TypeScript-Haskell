-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
import Data.Maybe (fromJust, isNothing)

sqr :: Float -> Float
sqr n = n * n

half :: Float -> Float
half = (/ 2)

hypotenuse :: Float -> Float -> Float
hypotenuse a b = sqrt $ sqr a + sqr b

hypotenuseEq :: Float -> Float
hypotenuseEq a = hypotenuse a a

newtype Circle = Circle {_circleRadius :: Float} deriving (Show, Eq)
newtype Square = Square {_squareSide :: Float} deriving (Show, Eq)

data Rectangle = Rectangle {_rectWeight :: Float, _reactHeight :: Float}
  deriving (Show, Eq)

data Donut = Donut {_donutOuterCircle :: Circle, _donutInnerCircle :: Circle}
  deriving (Show, Eq)

data Shape = Shape
  { _shpOuterShape :: OuterShape
  , _shpInnerShape :: Maybe Shape
  }
  deriving (Show)

data OuterShape = OuterShape
  { _outerShpCaseSquare :: Square
  , _outerShpInnerCircle :: Circle
  , _outerShpInnerSquare :: Square
  }
  deriving (Show)

class HasArea a where
  area :: a -> Float

instance HasArea Circle where
  area :: Circle -> Float
  area c = pi * sqr (_circleRadius c)

instance HasArea Rectangle where
  area :: Rectangle -> Float
  area rect = _rectWeight rect * _reactHeight rect

instance HasArea Square where
  area :: Square -> Float
  area sq = area $ Rectangle s s
   where
    s = _squareSide sq

instance HasArea Donut where
  area :: Donut -> Float
  area donut = area outCirc - area inCirc
   where
    outCirc = _donutOuterCircle donut
    inCirc = _donutInnerCircle donut

instance HasArea OuterShape where
  area :: OuterShape -> Float
  area shp = area innerCircle - area innerSquare
   where
    innerCircle = _outerShpInnerCircle shp
    innerSquare = _outerShpInnerSquare shp

instance HasArea Shape where
  area :: Shape -> Float
  area shp
    | isNothing innerShp = 0
    | otherwise = area outerShp + area (fromJust innerShp)
   where
    outerShp = _shpOuterShape shp
    innerShp = _shpInnerShape shp

type Radius = Float
mkDonut :: Radius -> Radius -> Donut
mkDonut r1 r2
  | r1 > r2 = Donut (Circle r1) (Circle r2)
  | otherwise = Donut (Circle r2) (Circle r1)

type ShapeFrameSide = Float
mkOuterShape :: ShapeFrameSide -> OuterShape
mkOuterShape frameSize =
  OuterShape
    (Square frameSize)
    (Circle halfFrameSide)
    (Square innerSquareSide)
 where
  innerSquareSide = hypotenuseEq halfFrameSide
  halfFrameSide = half frameSize

epsilon :: Float
epsilon = 0.0000000001

isEpsilon :: Float -> Bool
isEpsilon n = n < epsilon

innerSquareSide :: Float -> Float
innerSquareSide n = hypotenuseEq $ half n

mkShape :: ShapeFrameSide -> Shape
mkShape frameSize
  | isEpsilon frameSize = Shape (mkOuterShape frameSize) Nothing
  | otherwise =
      Shape
          (mkOuterShape frameSize)
          (Just (mkShape (innerSquareSide frameSize)))

shp :: OuterShape
shp = mkOuterShape 100
