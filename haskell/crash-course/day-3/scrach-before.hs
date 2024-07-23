sqr :: Float -> Float
sqr n = n * n

newtype Circle = Circle {radius :: Float}
  deriving (Show, Eq)

newtype Square = Square {side :: Float}
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
