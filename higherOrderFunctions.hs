import Data.Char

-- demo of filter (sum of even numbers using filter)
sumEven :: [Int] -> Int
sumEven xs = sum $ filter even xs

-- define filter recursively
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs)
	| f x = x : myFilter f xs
	| otherwise = myFilter f xs 

-- demo of Map (convert each char to uppercase using a lambda function
myUpper :: [Char] -> [Char]
myUpper xs = map (\x -> toUpper x) xs

-- define map recursively
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

--exercises for Chapter 7 (Programming in Haskell)
-- 7.2 define functions all, any, takeWhile and dropWhile
-- i) all
myAll :: (a -> Bool) -> [a] -> Bool
myAll f xs = take 1 ([False | x <- xs, not $ f x]) == [False]

-- ii) any
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = take 1 ([True | x <- xs, f x]) == [True]

-- iii) takeWhile
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f [] = []
myTakeWhile f (x:xs)
	| f x = x : myTakeWhile f xs
	| otherwise = myTakeWhile f []

-- iv) dropWhile
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f (x:xs)
	| f x = myDropWhile f xs
	| otherwise = (x:xs)

-- implementing sum using foldr
mySum :: (Num a) => [a] -> a
mySum xs = foldr (+) 0 xs

-- reverse using foldr
myReverse :: [a] -> [a]
myReverse xs = foldr (\x acc -> acc ++ [x]) [] xs

-- filter using foldr
myFilter1 :: (a -> Bool) -> [a] -> [a]
myFilter1 p xs = foldr (\x acc -> if p x then x:acc else acc) [] xs

-- 7.4. convert decimal number to Int => [2,3,4,5] -> 2345
dec2int :: [Int] => Int
dec2int xs = foldl (\acc x -> 10*acc + x) 0 xs








