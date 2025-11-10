import Data.Char (isAlpha, toLower)
import qualified Data.Map as Map

areAnagram :: String -> String -> Bool
areAnagram s1 s2 = elemsFreqMap cleaned1 == elemsFreqMap cleaned2
  where cleaned1 = cleanStr s1
        cleaned2 = cleanStr s2
        cleanStr = map toLower . filter isAlpha

elemsFreqMap :: (Eq a, Ord a) => [a] -> Map.Map a Int
elemsFreqMap xs = Map.fromListWith (+) [(e, 1) | e <- xs]
