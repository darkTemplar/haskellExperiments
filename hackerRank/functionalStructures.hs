import qualified Data.Foldable as F
import Data.Monoid

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- to enable fmap 
instance Functor Tree where
	fmap f EmptyTree = EmptyTree
	fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-- enable foldl and foldr and foldMap
instance F.Foldable Tree where
	foldMap f EmptyTree = mempty
	foldMap f (Node x left right) = (F.foldMap f left) `mappend` (f x) `mappend` (F.foldMap f right)



-- supply empty tree with first node
singletonTree :: a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

-- insert value into tree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singletonTree x
treeInsert x (Node a left right)
	| x == a = Node x left right
	| x > a = Node a left (treeInsert x right)
	| otherwise = Node a (treeInsert x left) right

-- check if value exists within tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right) 
	| x == a = True
	| x > a = treeElem x right
	| otherwise = treeElem x left 

-- insert list of values into an emptyTree
treeMultiInsert :: (Ord a) => [a] -> Tree a
treeMultiInsert xs = foldr treeInsert EmptyTree xs

-- pretty print Tree Data structure

-- count number of nodes in Tree 
treeSize :: Tree a -> Int 
treeSize EmptyTree = 0
treeSize (Node x left right) = 1 + (treeSize left) + (treeSize right)

-- inOrder traversal
inOrder :: Tree a -> [a]
inOrder tree = F.foldMap (\x -> [x]) tree 

-- number of BST's possible given "n" nodes
numBst :: Int -> Int
numBst 0 = 1
numBst 1 = 1
numBst 2 = 2
numBst n = sum $ [(numBst (x-1)) * (numBst (n-x))| x <- [1..n]]


-- postOrder traversal
--postOrder :: Tree a -> [a]




