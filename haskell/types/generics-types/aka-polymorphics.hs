data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a)
    deriving Show

singletonNode :: a -> BinaryTree a
singletonNode x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
treeInsert x EmptyTree = singletonNode x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

flatten :: BinaryTree a -> [a]
flatten EmptyTree = []
flatten (Node x left right) = [x] ++ flatten left ++ flatten right

nums :: [Int]
nums = [8, 6, 4, 1, 5, 5, 6, 7, 3, 5 {-| , 12, 18, 14 |-} ]

tree :: BinaryTree Int
tree = foldr treeInsert EmptyTree nums

flatTree :: [Int]
flatTree = flatten tree

countNodes :: BinaryTree a -> Int
countNodes t = length . flatten $ t
