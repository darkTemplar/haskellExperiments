--primitive getCh :: IO Char

getLine' :: IO String
getLine' = do
	x <- getChar
	if x == '\n' then return [] 
		else do
			xs <- getLine'
			return (x:xs)

abstractGetline :: IO String
abstractGetline = get []

get :: String -> IO String
get xs = do
	x <- getChar
	case x of
		'\n' -> return xs
		_  -> get (xs ++ [x])


putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
	putChar x
	putStr' xs

putStr1 :: String -> IO ()
putStr1 [] = return ()
putStr1 (x:xs) = putChar x >> putStr1 xs


putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr1 xs >> putChar '\n'





-- takes a string input from keyboard and displays it's length
strlen :: IO ()
strlen = do
	putStrLn "Enter string input:"
	xs <- getLine
	putStr "The string has "
	putStr (show(length xs))
	putStrLn " characters"

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"


type Pos = (Int, Int)

-- goto a particular position on the screen
goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H") 

-- combining multiple IO actions 

-- we discard results here after action has been performed
seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do
	a
	seqn as

seqn1 :: [IO a] -> IO ()
seqn1 ms = foldr (>>) (return ()) ms

-- putStr via seqn
seqnPutStr :: String -> IO ()
seqnPutStr xs = seqn1 [putChar x| x <- xs]

-- implementation of sequence
sequence' :: [IO a] -> IO [a]
sequence' [] = return[]
sequence' (m:ms) = m >>= \a ->
	do
		as <- sequence' ms
		return (a:as)  

-- implementation of mapM
mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' f ms = sequence' (map f ms) 


-- Implementing filterM -> will filter out certain elements of list
filterM' :: (a -> IO Bool) -> [a] -> IO [a]
filterM' p (m:ms) = do
	flag <- p m
	ys   <- filterM' p ms
	if flag then return (m:ys) else return ys










