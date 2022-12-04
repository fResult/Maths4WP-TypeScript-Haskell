-- Enum
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
    deriving Show
-- day = Sat :: Day
-- :t day = Day
-- show day -- "Sat"

data Shape = Circle Radius | Square Side
    deriving Show
-- Circle 20    -- 20.0
-- Square 20    -- 20.0
-- :t Circle    -- Float -> Shape
-- :t Circle 20 -- Shape

type Radius = Float
type Side = Float
type Area = Float

area :: Shape -> Area
area (Circle r) = pi * r * r
area (Square s) = s * s

-- data Person String String Int


data Person = Person String String Age
    deriving Show

type FirstName = String
type LastName = String
type Age = Int
data Person' = Person' FirstName LastName Age
    deriving Show
