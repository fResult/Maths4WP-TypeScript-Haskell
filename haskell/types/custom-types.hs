-- Enum
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
    deriving Show
-- day = Sat :: Day
-- :t day = Day
-- show day -- "Sat"

data Shape = Circle Float | Square Float
    deriving Show
-- Circle 20    -- 20.0
-- Square 20    -- 20.0
-- :t Circle    -- Float -> Shape
-- :t Circle 20 -- Shape
