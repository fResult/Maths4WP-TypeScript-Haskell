import Data.List

countUniqChars :: String -> Int
countUniqChars = length . group . sort
