-- import Data.List (delete, last)
data Queue a = Queue [a] deriving Show

createQueue :: a -> Queue a
createQueue x = Queue [x]

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs) = Queue (xs ++ [x])

dequeue :: Queue a -> Queue a
dequeue (Queue []) = error "Queue is empty!"
dequeue (Queue (_:xs)) = Queue xs

-- test :: [Int] -> [Int]
-- test xs = delete (last xs) xs

-- deletedLast :: [Int]
-- deletedLast = Data.List.delete (Data.List.last [1..10]) [1..10]
-- deletedLast = delete (last [1..10]) [1..10]