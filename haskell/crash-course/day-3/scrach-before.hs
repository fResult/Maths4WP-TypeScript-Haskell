sqr :: Float -> Float
sqr n = n * n

newtype Circle = Circle Float
  deriving (Show, Eq)

newtype Square = Square Float
  deriving (Show, Eq)

class HasArea a where
  area :: a -> Float

instance HasArea Circle where
  area :: Circle -> Float
  area (Circle r) = pi * sqr r

instance HasArea Square where
  area :: Square -> Float
  area (Square s) = sqr s

type FullName = String
type NickName = String
data Person = Person FullName NickName deriving (Show)
