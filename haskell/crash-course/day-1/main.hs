import Data.List

count_uniq_chars :: String -> Int
count_uniq_chars str = length . group . sort $ str
