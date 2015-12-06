module Main where
import Test.HUnit
import Network.HTTP
import System.IO
import Data.List.Split
import Data.List (nub)

doTests :: IO ()
doTests = do
	_ <- runTestTT $ TestList [ tday1 ]
	return ()


-- utility functions
stringToInt :: String -> Int
stringToInt x = read x :: Int

shiftByOne :: [a] -> [a]
shiftByOne (x:xs) = xs ++ [x] 

-- ***** Day 1 *****
tday1 :: Test
tday1 = TestList[ t1part1, t1part2 ]

-- part 1 : Depending on floor code given e.g. '(())', convert to floor number Santa should go to
-- '(' means + 1 floor and ')' means -1

floorCode :: String -> Int 
floorCode = foldr (\x acc -> if x == '(' then acc + 1 else acc - 1) 0
t1part1 :: Test
t1part1 = "floorCode" ~: TestList[floorCode "(())" ~?= 0,
								floorCode "(())" ~?= 0,
								floorCode "((()" ~?= 2]

-- part 2 : Find instruction number in floor code which makes Santa enter basement i.e. -1

findBasement :: String -> Int
findBasement cs = length $ takeWhile (/= -1) $ scanl (\acc x -> if x == '(' then acc + 1 else acc - 1) 0 cs
t1part2 :: Test
t1part2 = "findBasement" ~: TestList[findBasement ")" ~?= 1,
									findBasement "()())" ~?= 5]

-- **** DAY 2 *****
tday2 :: Test
tday2 = TestList[ t2part1, t2part2 ]

-- part 1 : Find least amount of wrapping paper needed to gift wrap a rectangular giftbox
-- we calculate this by surface area of box + area of smallest side (for slack)
-- input will be multiple lines in form of length X width X height and we need to find total gift wrap needed
giftWrapNeeded :: [[Int]] -> Int
giftWrapNeeded xs = foldr (\ys acc -> acc + (totalSurfaceArea ys) + (minSurfaceArea ys)) 0 xs
	where
		totalSurfaceArea ys = 2 * (sum $ zipWith (*) ys (shiftByOne ys))
		minSurfaceArea ys = minimum $ zipWith (*) ys (shiftByOne ys)
t2part1 :: Test
t2part1 = "giftWrapNeeded" ~: TestList[giftWrapNeeded [[2,3,4], [1,1,10]] ~?= 101]

-- part 2 : Amount of ribbon needed to wrap the gift = shortest perimeter of box + cubic volume of box
-- input will be in same format as above
ribbonNeeded :: [[Int]] -> Int
ribbonNeeded xs = foldr (\ys acc -> acc + (volume ys) + (minPerimeter ys)) 0 xs
	where
		volume ys = product ys
		minPerimeter ys = 2 * (minimum $ zipWith (+) ys (shiftByOne ys))
t2part2 :: Test
t2part2 = "ribbonNeeded" ~: TestList[ribbonNeeded [[2,3,4], [1,1,10]] ~?= 48]

-- read input from webpage (won't work since requires authentication)
readInputFromWeb :: IO ()
readInputFromWeb = do
	rsp <- simpleHTTP (getRequest "http://adventofcode.com/day/2/input")
	let body = fmap rspBody rsp
	print body

-- read puzzle 2 input from text file, transform to list of tuples and apply giftWrapNeeded function to get final answer
puzzle2 :: IO ()
puzzle2 = do
	withFile "adventOfCodeDay2.txt" ReadMode (\handle -> do
		contents <- hGetContents handle
		--print $ giftWrapNeeded $ map (map stringToInt . (splitOn "x")) $ lines contents)
		print $ ribbonNeeded $ map (map stringToInt . (splitOn "x")) $ lines contents)

-- ***** Day 3 *****
tday3 :: Test
tday3 = TestList[ t3part1 ]

-- part 1: count number of houses where Santa leaves at least one present (i.e. number of houses visited by Santa)
-- input will be in form of directions :  '^' - North, '>' - East, '<' - West and so on.
-- we will parse these into (x,y) coordinate tuples
housesVisited :: [(Int, Int)] -> Int
housesVisited xs = length . nub $ scanl (\(x, y) (a,b) -> (x+a, y+b)) (0,0) xs
t3part1 :: Test
t3part1 = "housesVisited" ~: TestList[housesVisited [(1,0)] ~?= 2,
									housesVisited [(0,1), (1,0), (0,-1), (-1,0)] ~?= 5]

puzzle3 :: IO ()
puzzle3 = do
	withFile "adventOfCodeDay3.txt" ReadMode (\handle -> do
		contents <- hGetContents handle 
		let parseDirections dir
			| dir == '>' = (1,0)
			| dir == '<' = (-1,0)
			| dir == '^' = (0,1)
			| dir == 'v' = (0,-1)
			| otherwise = (0,0)
		print $ housesVisited $ foldr (\x acc -> (parseDirections x):acc) [] contents)






