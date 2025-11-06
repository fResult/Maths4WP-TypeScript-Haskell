data Flag = No | Yes
    deriving (Show, Ord, Eq)

toString :: Flag -> String
toString Yes = "Yes"
toString No = "No"

not' :: Flag -> Flag
not' b
    | b == Yes = No
    | otherwise = Yes

-- instance Show Flag where
--     show :: Flag -> String
--     show = toString

-- instance Eq Flag where
--     Yes == Yes = True
--     No == No = True
--     _ == _ = False

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Show, Eq)
