import Data.Char
-- to suppress something from prelude use the following
--import Prelude hiding ((||))

-- find positive factors of a number n
factors :: Int -> [Int] 
factors n = [x| x <- [1..n], n `mod` x == 0]

-- determine if a given positive number is prime
isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

-- Caesar cipher implementation
let2Int :: Char -> Int
let2Int c = ord c - ord 'a' 

int2Let :: Int -> Char
int2Let n = chr(ord 'a' + n)

-- chapter 5 exercises
-- 5.1 -> sum of first n integer squares
squares :: Int -> Int
squares n = sum [x^2 | x <- [1..n]]

-- 5.2 -> implement library function replicate
myReplicate :: Int -> a -> [a]
myReplicate n x = [x| _ <- [1..n]]

-- 5.3 -> output all pythagorean triplets (x,y,z) where x,y and z are bounded by a number n
pyths :: Int => [(Int,Int,Int)]
pyths n = [(x,y,z)| x <- [1..n], y <- [1..n], z <- [1..n], isPyth x y z] 

-- helper function for above. Determines if 3 numbers form a pythagorean triplet
isPyth :: Int -> Int -> Int -> Bool
isPyth x y z = (x^2 + y^2) == z^2

-- Chapter 6 exercises
-- 6.4 define a recursive merge function to merge 2 sorted arrays
recMerge :: (Ord a) => [a] -> [a] -> [a]
recMerge [] xs = xs
recMerge xs [] = xs
recMerge (z:zs) (y:ys) 
	| z < y = z : recMerge zs (y:ys)
	| otherwise = y : recMerge (z:zs) ys 

-- 6.5 merge sort
msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = let (as,bs) = halves xs
	in recMerge (msort as) (msort bs)

-- helper function for merge sort to halve an array
halves :: [a] -> ([a],[a])
halves xs = (take n xs, drop n xs)
	where n = (length xs) `div` 2 

-- quick sort 
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
	where 
		smaller = [y | y <- xs, y <= x]
		larger = [y | y <- xs, y > x]

-- reverse qsort
revQsort :: (Ord a) => [a] -> [a]
revQsort [] = []
revQsort (x:xs) = revQsort larger ++ [x] ++ revQsort smaller
	where 
		smaller = [y | y <- xs, y < x]
		larger = [y | y <- xs, y >= x]

-- qsort variants
div' :: Int
div' = x `div` length xs
	where
		x = 10
		xs = [1..5]

-- list comprehension assignments
replicate' :: Int -> a -> [a]
replicate' n a = [a | _ <- [1..n]]

-- integers which are "perfect" i.e. sum of factors (excluding number itself) is the number. E.g. 28 = 1 + 2 + 4 + 7 + 14

-- scalar product
scalar xs ys = sum [x * y | (x,y) <- xs `zip` ys]

-- reimplement factors 
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

-- find function (find all values which match the key assuming list of [(key, value)] tuples)
find :: (Eq a) => a -> [(a,b)] -> [b]
find x xs = [v | (k,v) <- xs, x == k]

-- positions (find all indexes which match value)
positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [k | (v,k) <- zip xs [0..n], v == x]
	where n = length xs - 1  

-- positions using find function defined above
positions' :: (Eq a) => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0..n])
	where n = length xs - 1








