-- Exercise 3: Stalin Sort Functions

stalinSort :: Ord a => [a] -> [a]
stalinSort []  = []
stalinSort [x] = [x]
stalinSort (x:y:xs)
  | x <= y      = x : stalinSort (y:xs)
  | otherwise   = stalinSort (x:xs)

-- Example trace:
-- x= 3, y= 1 ->  3 ≤ 1  ❌ -> stalinSort(3:[2,4,3,7,1,9,10,11,2,4])
-- x= 3, y= 2 ->  3 ≤ 2  ❌ -> stalinSort(3:[4,3,7,1,9,10,11,2,4])
-- x= 3, y= 4 ->  3 ≤ 4  ✅ -> 3:stalinSort(4:[3,7,1,9,10,11,2,4])
-- x= 4, y= 3 ->  4 ≤ 3  ❌ -> stalinSort(3:4:[7,1,9,10,11,2,4])
-- x= 4, y= 7 ->  4 ≤ 7  ✅ -> 3:4:stalinSort(7:[1,9,10,11,2,4])
-- x= 7, y= 1 ->  7 ≤ 1  ❌ -> stalinSort(3:4:7:[9,10,11,2,4])
-- x= 7, y= 9 ->  7 ≤ 9  ✅ -> 3:4:7:stalinSort(9:[10,11,2,4])
-- x= 9, y=10 ->  9 ≤ 10 ✅ -> 3:4:7:9:stalinSort(10:[11,2,4])
-- x=10, y=11 -> 10 ≤ 11 ✅ -> 3:4:7:9:10:stalinSort(11:[2,4])
-- x=11, y= 2 -> 11 ≤ 2  ❌ -> stalinSort(3:4:7:9:10:11:[4])
-- [x]                      -> 3:4:7:9:10:11:[4]
