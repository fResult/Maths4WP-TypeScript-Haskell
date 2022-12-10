import Data.List (delete, last)
data Queue a = Queue [a] deriving Show

createQueue :: a -> Queue a
createQueue x = Queue [x]

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs) = Queue (xs ++ [x])

dequeue :: Eq a => Queue a -> Queue a
dequeue (Queue []) = error "Queue is empty!"
dequeue (Queue xs) = Queue (Data.List.delete (last xs) xs)

-- test :: [Int] -> [Int]
-- test xs = delete (last xs) xs

-- deletedLast :: [Int]
-- deletedLast = Data.List.delete (Data.List.last [1..10]) [1..10]
-- deletedLast = delete (last [1..10]) [1..10]

enqueueLeft :: a -> Queue a -> Queue a
enqueueLeft x (Queue xs) = Queue (x:xs)

dequeueLeft :: Queue a -> Queue a
dequeueLeft (Queue []) = error "Dequeue Empty List"
dequeueLeft (Queue (_:xs)) = Queue xs