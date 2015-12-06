{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE ParallelListComp #-}

module Main where
import Prelude hiding (takeWhile,all)
import Test.HUnit      -- unit test support

import XMLTypes        -- support file for XML problem (provided)
import Play            -- support file for XML problem (provided)

doTests :: IO ()
doTests = do 
  _ <- runTestTT $ TestList [ testFoldr, testTree ]
  return ()

main :: IO ()
main = do 
       doTests
       return ()

----------------------------------------------------------------------

testFoldr :: Test
testFoldr = TestList [tintersperse, tinvert, ttakeWhile, tfind, tall]

-- The intersperse function takes an element and a list 
-- and `intersperses' that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"

intersperse ::  a -> [a] -> [a]
intersperse n xs = tail $ foldr (\x acc -> n:x:acc) [] xs 
tintersperse :: Test
tintersperse = "intersperse" ~: TestList[intersperse ',' "abcde" ~?= "a,b,c,d,e",
                                          intersperse 0 [1..5] ~?= [1,0,2,0,3,0,4,0,5],
                                          intersperse ',' "" ~?= ""]


-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)] returns [(1,"a"),(2,"a")] 

invert :: [(a,b)] -> [(b,a)]
invert xs = foldr (\(x,y) acc -> (y,x):acc) [] xs
tinvert :: Test
tinvert = "invert" ~: TestList[invert [(1, "a"), (2, "b")] ~?= [("a", 1), ("b", 2)]]
 

-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p xs = foldr (\x acc -> if p x then x:acc else acc) [] xs 
ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: TestList[takeWhile (>3) [1..5] ~?= [4,5]]
 

-- find pred lst returns the first element of the list that 
-- satisfies the predicate. Because no element may do so, the 
-- answer is returned in a "Maybe".
-- for example: 
--     find odd [0,2,3,4] returns Just 3

find :: (a -> Bool) -> [a] -> Maybe a
find p xs = case result of
  [] -> Nothing
  rs -> Just $ head rs
  where result = foldr (\x acc -> if p x then x:acc else acc) [] xs
tfind :: Test
tfind = "find" ~: TestList[find (<2) [1..5] ~?= Just 1,
                            find (>10) [1..5] ~?= Nothing]
 

-- all pred lst returns False if any element of lst 
-- fails to satisfy pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

all  :: (a -> Bool) -> [a] -> Bool
all p xs = foldr (\x acc -> (p x) && acc) True xs
tall :: Test
tall = "all" ~: TestList[all even [2,4,6] ~?= True,
                          all odd [1..3] ~?= False]

-- EXTRA CREDIT
-- implement zip using foldr
-- for example:
--       zip' [1..3] [4..6] = [(1,4), (2,5), (3,6)]

zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys = [f y | y <- ys | f <- partialZippedList]
  where go a b = (a,b)
        partialZippedList = foldr (\x acc -> (go x) : acc) [] xs


-- EXTRA CREDIT
-- implement foldl using foldr

----------------------------------------------------------------------

testTree :: Test
testTree = TestList [ tinvertTree, ttakeWhileTree, tallTree ]

-- | a basic tree data structure
data Tree a = Leaf | Branch a (Tree a) (Tree a) deriving (Show, Eq)

foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree e _ Leaf     = e
foldTree e n (Branch a n1 n2) = n a (foldTree e n n1) (foldTree e n n2)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree Leaf (\x t1 t2 -> Branch (f x) t1 t2) 

-- The invertTree function takes a tree of pairs and returns a new tree 
-- with each pair reversed.  For example:
--     invertTree (Branch ("a",1) Leaf Leaf) returns Branch (1,"a") Leaf Leaf

invertTree :: Tree (a,b) -> Tree (b,a)
invertTree = mapTree (\(x, y) -> (y, x))
tinvertTree :: Test
tinvertTree = "invertTree" ~: TestList[invertTree (Branch ("a",1) Leaf Leaf) ~?= Branch (1,"a") Leaf Leaf]
 

-- takeWhileTree, applied to a predicate p and a tree t, 
-- returns the largest prefix tree of t  (possibly empty) 
-- where all elements satisfy p. 
-- For example, given the following tree

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Leaf Leaf) (Branch 3 Leaf Leaf)

--     takeWhileTree (< 3) tree1  returns Branch 1 (Branch 2 Leaf Leaf) Leaf
--     takeWhileTree (< 9) tree1  returns tree1
--     takeWhileTree (< 0) tree1  returns Leaf

takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree p = foldTree Leaf (\x left right -> if p x then Branch x left right else Leaf)
ttakeWhileTree :: Test
ttakeWhileTree = "takeWhileTree" ~: TestList[takeWhileTree (< 3) tree1 ~?= Branch 1 (Branch 2 Leaf Leaf) Leaf,
                                              takeWhileTree (< 9) tree1 ~?= tree1,
                                              takeWhileTree (< 0) tree1 ~?= Leaf]
 

-- allTree pred tree returns False if any element of tree 
-- fails to satisfy pred and True otherwise.
-- for example:
--    allTree odd tree1 returns False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree p = foldTree True (\x left right -> (p x) && left && right)
tallTree :: Test
tallTree = "allTree" ~: TestList[allTree odd tree1 ~?= False]
 

-- WARNING: This one is a bit tricky!  (Hint: the value
-- *returned* by foldTree can itself be a function.)

-- map2Tree f xs ys returns the tree obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one branch is longer than the other, then the extra elements 
-- are ignored.
-- for example:
--    map2Tree (+) (Branch 1 Leaf (Branch 2 Leaf Leaf)) (Branch 3 Leaf Leaf)
--        should return (Branch 4 Leaf Leaf)

map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2Tree = undefined 

tmap2Tree :: Test
tmap2Tree = "map2Tree" ~: assertFailure "testcase for map2Tree"

-- zipTree takes two trees and returns a tree of corresponding pairs. If
-- one input branch is smaller, excess elements of the longer branch are
-- discarded.
-- for example:  
--    zipTree (Branch 1 (Branch 2 Leaf Leaf) Leaf) (Branch True Leaf Leaf) returns 
--            (Branch (1,True) Leaf Leaf)

-- To use foldTree, you'll need to think about this one in
-- the same way as part (d).

zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree = undefined

tzipTree :: Test
tzipTree = "zipTree" ~: assertFailure "testcase(s) for zipTree"

----------------------------------------------------------------------




formatPlay :: SimpleXML -> SimpleXML
formatPlay = error "implement formatPlay"
 




firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds) 
    | c==d = firstDiff cs ds 
    | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)

-- | Test the two files character by character, to determine whether
-- they match.
testResults :: String -> String -> IO ()
testResults file1 file2 = do 
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> return ()
    Just (cs,ds) -> assertFailure msg where
      msg  = "Results differ: '" ++ take 20 cs ++ 
            "' vs '" ++ take 20 ds

testXML :: Test
testXML = TestCase $ do 
  writeFile "dream.html" (xml2string (formatPlay play))
  testResults "dream.html" "sample.html"

