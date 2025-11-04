xs :: [Int]
xs = [1..]

main :: IO ()
main = do
  print $ take 100 xs
