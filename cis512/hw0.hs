import Test.HUnit
import Data.Char

-- We will play with tests and functions here


-- 1. list generator. Given a value 'a' creates list with 'n' copies

testClone1, testClone2, testClone3 :: Test
testClone1 = clone 'a' 4 ~?= ['a', 'a', 'a', 'a']
testClone2 = clone 'a' 0 ~?= []
testClone3 = clone 1.1 3 ~?= [1.1, 1.1, 1.1]

clone :: a -> Int-> [a]
clone x 0 = []
clone x n = x : clone x (n-1)

runCloneTests :: IO Counts
runCloneTests = runTestTT(TestList([testClone1, testClone2, testClone3]))


-- 2. convert string to uppercase
toUpperString :: String -> String
toUpperString [] = []
toUpperString (c:cs) = toUpper c : toUpperString cs


-- 3. map' - 
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- 4. filter'
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x then x : filter' f xs else filter' f xs

-- 4. maximum and reverse using fold
maximum' :: Ord a => [a] -> a
maximum' = foldl1 (\acc y -> if acc > y then acc else y)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []








