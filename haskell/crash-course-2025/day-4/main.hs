import Data.List (sort)

factors :: Int -> [Int]
factors n = [ i | i <- [1..n], n `mod` i == 0 ]

factors' :: Int -> [Int]
factors' n =  sort (mods ++ divs)
  where
    mods = [ i | i <- [1..(floor . sqrt . fromIntegral) n], n `mod` i == 0 ]
    divs = [ d | i <- mods, let d = n `div` i, i /= d ]

-- Very stupid prime number checker
isPrime :: Int -> Bool
isPrime n = factors' n == [1, n]


-- Test data
-- To have profiler, use `:set +s` in GHCi
m1, m10, m100, m1000, m10000 :: Int
m1     = 1_000_000
m10    = m1 * 10
m100   = m10 * 10
m1000  = m100 * 10
m10000 = m1000 * 10

{-- Define Our Own Types --}
data Boolean = Yes | No

instance Eq Boolean where
  (==) :: Boolean -> Boolean -> Bool
  Yes == Yes = True
  No  == No  = True
  _ == _  = False

instance Show Boolean where
  show :: Boolean -> String
  show Yes = "Yes"
  show No  = "No"

data Boolean' = Yes' | No' deriving (Eq, Show)

not' :: Boolean -> Boolean
not' Yes = No
not' No  = Yes

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show)
-- It is able to sort:
---- λ> sort [Mon, Wed, Tue, Fri, Thu]
---- [Mon,Tue,Wed,Thu,Fri]
