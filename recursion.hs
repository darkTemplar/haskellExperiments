-- recursion HW FP101

-- and function definitions
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = and xs && x

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss