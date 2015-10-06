import Control.Monad

-- repeat Hello World multiple times
hello_worlds n = do 
					when(n > 0) $ do
						putStrLn "Hello World"
						hello_worlds (n-1)



-- replicate elements of an Integer list
-- Usage: replicateList 2 [1..4] = [1,1,2,2,3,3]
replicateList :: Int -> [Int] -> [Int]
replicateList _ [] = []
replicateList 0 _ = []
replicateList n (x:xs) = replicate n x ++ replicateList n xs 


-- implement filter function
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\x acc -> if f x then x:acc else acc) [] xs

-- remove elems at even indexes
removeEvenPositions :: [Int] -> [Int]
removeEvenPositions [] = []
removeEvenPositions xs = [x | (x,y) <- enumeratedList, even y] where
	enumeratedList = zip xs [1..length xs]

-- implement reverse
reverse' :: [a] -> [a]
reverse' xs = foldl (\acc x -> x : acc) [] xs

-- sum of odd elements
sumOddElems :: Integral a => [a] -> a
sumOddElems xs = sum [x | x <- xs, odd x]

-- length of arrays without using library functions
length' :: [a] -> Int
length' xs = sum [1 | x <- xs]

-- update list with absolute values
absoluteValues :: Num a => [a] -> [a]
absoluteValues xs = [abs (x) | x <- xs]

-- exponential evaluation by using first 10 members of the infinite series
exponential :: (Ord a, Enum a, Floating a) => a -> a
exponential n = sum $ take 10 [(n**x)/factorial(x) | x <- [0..]] where
	factorial y =  if y > 0 
		then product [k | k <- [1..y]]
		else 1




--main = do
--	hello_worlds 4
