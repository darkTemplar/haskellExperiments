import Test.HUnit

-- define data type and functions to define days in a week

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq)

nextDayOfWeek :: Day -> Day
nextDayOfWeek Monday    = Tuesday
nextDayOfWeek Tuesday   = Wednesday
nextDayOfWeek Wednesday = Thursday
nextDayOfWeek Thursday  = Friday
nextDayOfWeek Friday    = Monday
nextDayOfWeek Saturday  = Monday
nextDayOfWeek Sunday    = Monday

twoBusinessDays :: Day -> Day
twoBusinessDays d = nextDayOfWeek $ nextDayOfWeek d

-- defining a non empty integer list data type
data NEIntList = Isingle Int | ICons Int NEIntList

oneTwoThree :: NEIntList
oneTwoThree = ICons 1 (ICons 2 (Isingle 3))

sumOfNEIntList :: NEIntList -> Int
sumOfNEIntList (Isingle x) = x
sumOfNEIntList (ICons x xs) = x + sumOfNEIntList xs


-- defining data types for trees
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- example tree
exTree :: Tree Int
exTree = Node 5 (Node 2 (Node 1 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 9 Leaf (Node 7 Leaf Leaf))

-- add elem to each node of tree
treePlus :: Tree Int -> Int -> Tree Int
treePlus Leaf _ = Leaf
treePlus (Node y leftTree rightTree) x = Node (y + x) (treePlus leftTree x) (treePlus rightTree x)

-- map function for tree
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Leaf = Leaf
treeMap f (Node x leftTree rightTree) = Node (f x) (treeMap f leftTree) (treeMap f rightTree)