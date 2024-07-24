import Control.DeepSeq (rnf2)
import Distribution.Simple.Program (c2hsProgram)

sqr :: Float -> Float
sqr n = n * n

newtype Circle = Circle {radius :: Float}
  deriving (Show, Eq)

newtype Square = Square {side :: Float}
  deriving (Show, Eq)

data Rectangle = Rectangle {weight :: Float, height :: Float}
  deriving (Show, Eq)

class HasArea a where
  area :: a -> Float

instance HasArea Circle where
  area :: Circle -> Float
  area c = pi * sqr (radius c)

instance HasArea Square where
  area :: Square -> Float
  area s = sqr (side s)

data HasArea' = Circle' Float | Square' Float deriving (Show)

area' :: HasArea' -> Float
area' (Circle' r) = pi * sqr r
area' (Square' s) = sqr s

data Donut = Donut
  { outerCircle :: Circle
  , innerCircle :: Circle
  }
  deriving (Show, Eq)

mkDonut :: Float -> Float -> Donut
mkDonut r1 r2
  | r1 > r2 = Donut (Circle r1) (Circle r2)
  | otherwise = Donut (Circle r2) (Circle r1)

mkDonut' :: Circle -> Circle -> Donut
mkDonut' c1 c2
  | radius c1 > radius c2 = Donut c1 c2
  | otherwise = Donut c2 c1

instance HasArea Donut where
  area :: Donut -> Float
  area donut = area (outerCircle donut) - area (innerCircle donut)

data Nat = Zero | Succ Nat
  deriving (Show, Eq, Ord)

instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  m + Zero = m
  m + (Succ n) = Succ (m + n)

  (*) :: Nat -> Nat -> Nat
  m * Zero = Zero
  m * (Succ n) = m * n + m

toInt :: Nat -> Integer
toInt Zero = 0
toInt (Succ n) = 1 + toInt n

data Stack a = EmptyStack | Stack a (Stack a)
  deriving (Show)

push :: a -> Stack a -> Stack a
push = Stack

pop :: Stack a -> Maybe (Stack a)
pop EmptyStack = Nothing
pop (Stack _ stk) = Just stk

top :: Stack a -> Maybe a
top EmptyStack = Nothing
top (Stack x _) = Just x

newtype StackL a = StackL [a] deriving (Show)
push' :: a -> StackL a -> StackL a
push' x (StackL xs) = StackL (x : xs)

pop' :: StackL a -> Maybe (StackL a)
pop' (StackL []) = Nothing
pop' (StackL (_ : xs)) = Just $ StackL xs

top' :: StackL a -> Maybe a
top' (StackL []) = Nothing
top' (StackL (x : _)) = Just x

-- type FullName = String
-- type NickName = String
-- data Person = Person FullName NickName deriving (Show)

-- type MidtermScore = Int
-- type FinalScore = Int
-- type HomeworkScore = Int
-- type ProjectScore = Int
-- data Score = Score MidtermScore FinalScore HomeworkScore ProjectScore
--   deriving (Show)

-- midtermScore :: Score -> MidtermScore
-- midtermScore (Score m _ _ _) = m

-- finalScore :: Score -> MidtermScore
-- finalScore (Score _ f _ _) = f

data Score = Score
  { midtermScore :: Int
  , finalScore :: Int
  , homeworkScore :: Int
  , projectScore :: Int
  }
  deriving (Show)

totalScore :: Score -> Int
totalScore s = midtermScore s + finalScore s + homeworkScore s + projectScore s

instance Eq Score where
  (==) :: Score -> Score -> Bool
  s1 == s2 = totalScore s1 == totalScore s2
