newtype Circle = Circle {radius :: Float} deriving (Show, Eq)
newtype Square = Square {side :: Float} deriving (Show, Eq)

data Rectangle = Rectangle {weight :: Float, height :: Float}
  deriving (Show, Eq)

data Donut = Donut {outerCircle :: Circle, innerCircle :: Circle}
  deriving (Show, Eq)

sqr :: Float -> Float
sqr n = n * n

class HasArea a where
  area :: a -> Float

instance HasArea Circle where
  area :: Circle -> Float
  area c = pi * sqr (radius c)

instance HasArea Rectangle where
  area :: Rectangle -> Float
  area rect = weight rect * height rect

instance HasArea Square where
  area :: Square -> Float
  area sq = area $ Rectangle s s
   where
    s = side sq

instance HasArea Donut where
  area :: Donut -> Float
  area donut = area outCirc - area inCirc
   where
    outCirc = outerCircle donut
    inCirc = innerCircle donut

type Radius = Float
mkDonut :: Radius -> Radius -> Donut
mkDonut r1 r2
  | r1 > r2 = Donut (Circle r1) (Circle r2)
  | otherwise = Donut (Circle r2) (Circle r1)
