gcd' :: Integral a => a -> a -> a
gcd' x y
	| x == y = x
	| x > y = gcd' (x-y) y
	| otherwise = gcd' x (y-x)

-- standard fibonacci implementation
fibonacci :: Integral a => a -> a
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

-- pascal's triangle
--pascals :: Integral a => a -> [a]
--pascals x = foldr (\x acc -> [comb x y | y <- [0..x]] : acc) [] [0..x] where
--	comb x y = 

-- sum of digits
sumOfDigits :: Integral a => a -> a
sumOfDigits x 
	| x < 10 = x
	| otherwise = (x `mod` 10) + sumOfDigits (x `div` 10)

-- super digit (We find super digit of number n after concatenating it k times)
--superDigit :: Integral a => a -> a -> a
--superDigit n k
--	| n < 10 = n
--	| otherwise = superDigit (sumOfDigits n k) where 
--		sumOfDigits x = foldr (\acc x -> )   


