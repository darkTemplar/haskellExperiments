import Data.Char

all' :: (a -> Bool) -> [a] -> Bool
all' p = not . any (not . p)

any' :: (a -> Bool) -> [a] -> Bool
any' p xs = not (all (\ x -> not (p x)) xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p (x:xs)
	| p x = x : takeWhile' p xs
	| otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
	| p x = dropWhile' p xs
	| otherwise = x:xs

-- foldl implementation of map
mapl :: (a->b) -> [a] -> [b]
mapl f xs = foldl (\ acc x -> acc ++ [f x]) [] xs

-- foldl implementation of filter
filterl :: (a -> Bool) -> [a] -> [a]
filterl p xs = foldl (\ acc x -> if p x then acc ++ [x] else acc) [] xs

dec2int :: [Int] -> Int
dec2int xs = foldl (\ acc x -> 10*acc + x) 0 xs


compose :: [a -> a] -> (a -> a)
compose = foldr (.) id 

sumsqreven = compose [sum, map(^2), filter even] 
