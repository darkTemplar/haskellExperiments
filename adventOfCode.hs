module Main where
import Test.HUnit
import Network.HTTP
import System.IO
import Data.Char
import Data.List
import Data.List.Split
import Crypto.Hash.MD5
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Regex.PCRE

doTests :: IO ()
doTests = do
	_ <- runTestTT $ TestList [ tday1, tday2, tday3, tday20 ]
	return ()


-- utility functions
stringToInt :: String -> Int
stringToInt x = read x :: Int

shiftByOne :: [a] -> [a]
shiftByOne (x:xs) = xs ++ [x]

tuplify2 :: [a] -> (a, a)
tuplify2 (a:b:[]) = (a, b) 

-- split list by alternate elements i.e. [1,2,3,4] -> ([1,3], [2,4])
splitListInTwo :: [a] -> ([a], [a])
splitListInTwo [] = ([], [])
splitListInTwo (x:y:zs) = ((x:xs), (y:ys)) where (xs, ys) = splitListInTwo zs

toDigits :: (Integral a) => a -> [a]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

toNumber :: (Integral a) => [a] -> a
toNumber xs = foldl' (\acc x -> x + (10 * acc)) 0 xs

-- **** Day 1 ****
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

-- **** DAY 2 ****
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

-- **** Day 3 ****
tday3 :: Test
tday3 = TestList[ t3part1, t3part2 ]

-- part 1: count number of houses where Santa leaves at least one present (i.e. number of houses visited by Santa)
-- input will be in form of directions :  '^' - North, '>' - East, '<' - West and so on.
-- we will parse these into (x,y) coordinate tuples
housesVisited :: [(Int, Int)] -> Int
housesVisited xs = length . nub $ scanl (\(x, y) (a,b) -> (x+a, y+b)) (0,0) xs
t3part1 :: Test
t3part1 = "housesVisited" ~: TestList[housesVisited [(1,0)] ~?= 2,
									housesVisited [(0,1), (1,0), (0,-1), (-1,0)] ~?= 5]

-- part 2: now Santa and Robo Santa take turns in visiting houses i.e. alternating coordinating tuples belong to Santa and Robo Santa
housesVisited2 :: [(Int,Int)] -> Int
housesVisited2 zs = length . nub $ generatePath xs ++ generatePath ys
	where
		generatePath xs = scanl (\(x, y) (a,b) -> (x+a, y+b)) (0,0) xs
		(xs, ys) = splitListInTwo zs
t3part2 :: Test
t3part2 = "housesVisited2" ~: TestList[housesVisited2 [(0,1), (0,-1)] ~?= 3,
										housesVisited2 [(0,1), (1,0), (0,-1), (-1,0)] ~?= 3]


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
		--print $ housesVisited $ foldr (\x acc -> (parseDirections x):acc) [] contents)
		print $ housesVisited2 $ foldr (\x acc -> (parseDirections x):acc) [] contents)

-- **** Day 4 ****
--tday4 :: Test
--tday4 = TestList[ t4part1 ]

-- part 1 :  Find lowest positive number which produces md5 hash which in hex has 5 leading 0's.
-- md5 hash to mine adventCoins is to be produced by combining secret key (input) and appending a decimal number to it
--adventCoinMining :: String -> Int

-- **** Day 6 ****

type Point = (Int, Int)
type Grid a = M.Map (Int, Int) a

-- part 1 : Number of lights on. set lighting according to Santa's instructions and count how many lights are finally lit in 1000 x 1000 Grid

-- generate 2D light grid i.e. Map (Int, Int) Bool based on number of lights in each dimension. All lights are initially off i.e. False

lightGrid :: Point -> Grid Bool
lightGrid (x, y) = M.fromList $ zip [(a,b)| a <- [0..x-1], b <- [0..y-1]] $ cycle [False]

lightOn :: Point -> Grid Bool -> Grid Bool
lightOn (x, y) grid = M.insert (x, y) True grid 

lightOff :: Point -> Grid Bool -> Grid Bool
lightOff (x, y) grid = M.insert (x, y) False grid

lightFlip :: Point -> Grid Bool -> Grid Bool
lightFlip p grid = case M.lookup p grid of
	Just True -> M.insert p False grid
	_ -> M.insert p True grid

lightOp :: String -> (Point -> Grid Bool -> Grid Bool)
lightOp f = case f of 
	"on" -> lightOn
	"off" -> lightOff
	"toggle" -> lightFlip

countLightsOn :: Grid Bool -> Int
countLightsOn grid = M.size . M.filter (== True) $ grid

-- for part 2, the lights have brightness levels instead of just On/Off (on -> +1, off -> -1 (to a min of zero) and toggle -> +2)

brightGrid :: Point -> Grid Int
brightGrid (x, y) = M.fromList $ zip [(a,b)| a <- [0..x-1], b <- [0..y-1]] $ cycle [0]

brightOn :: Point -> Grid Int -> Grid Int
brightOn p grid = case M.lookup p grid of
	Nothing -> M.insert p 1 grid
	Just x -> M.insert p (x+1) grid 

brightOff :: Point -> Grid Int -> Grid Int
brightOff p grid = case M.lookup p grid of
	Nothing -> M.insert p 0 grid
	Just x -> M.insert p (max 0 (x-1)) grid

brightFlip :: Point -> Grid Int -> Grid Int
brightFlip p grid = case M.lookup p grid of
	Nothing -> M.insert p 2 grid
	Just x -> M.insert p (x+2) grid

brightOp :: String -> (Point -> Grid Int -> Grid Int)
brightOp f = case f of 
	"on" -> brightOn
	"off" -> brightOff
	"toggle" -> brightFlip

brightness :: Grid Int -> Int
brightness grid = (sum . M.elems) grid

-- applies light operation function on a light grid on rectangular area given by two tuples
gridOperation :: (Point -> Grid a -> Grid a) -> Grid a -> (Point, Point) -> Grid a
gridOperation f grid (p1,p2) = foldr (\(x,y) acc -> f (x,y) acc) grid $ bounds p1 p2 where
	bounds (a,b) (c,d) = [(x, y) | x <- (boundRange a c) , y <- (boundRange b d)]
	boundRange x y = if x <= y then [x..y] else [y..x]

parseGridOp :: String -> (String, (Point, Point))
parseGridOp line = (fname, (p1, p2)) where
	fname = opList !! 0
	p1 = parseNum $ opList !! 1
	p2 = parseNum $ opList !! 3
	opList = (\xs -> if head xs == "turn" then tail xs else xs) . words $ line
	parseNum numStr = tuplify2 $ map (\x -> read x :: Int) $ splitOn "," numStr


lightsOn :: IO ()
lightsOn = do
	withFile "adventOfCodeDay6.txt" ReadMode (\handle -> do
		contents <- hGetContents handle
		-- part 1
		--print $ countLightsOn $ foldl' (\grid (f,(p1, p2)) -> gridOperation (lightOp f) grid (p1, p2)) M.empty (map parseGridOp $ lines contents)
		-- part 2
		print $ brightness $ foldl' (\grid (f,(p1, p2)) -> gridOperation (brightOp f) grid (p1, p2)) M.empty (map parseGridOp $ lines contents)
		)


-- **** Day 7 ****
-- circuit builder
-- part 1 - complete circuit and logic gate operations and provide final signals at each gate


-- **** Day 10 ****
-- say out the number LOUD!! 
-- e.g. 1211 is one 1, one 2 and 2 one's and hence it becomes 111221

-- without using group function 
sayNumber :: [Int] -> [Int]
sayNumber xs = concatMap (\(a,b) -> [a,b]) $ foldr appendNum [] xs where
	appendNum n [] = [(1, n)]
	appendNum n ((x,y):xs) = if n == y then ((x+1), y):xs else (1, n):(x, y):xs        

-- using group function from Data.List package
sayNumber1 :: [Int] -> [Int]
sayNumber1 xs = concatMap (\ys -> [length ys, ys !! 0]) $ group xs

lookAndSay :: IO ()
lookAndSay = do
	print $ length $ foldr (.) id (replicate 50 sayNumber1) $ toDigits 3113322113
	print $ length $ foldr (.) id (replicate 50 sayNumber) $ toDigits 3113322113


-- **** Day 14 ****
-- Reindeer Olympics
-- part 1 : each reindeer has a speed and rest time associated with it
-- given input time, find which reindeer has travelled furthest

distance :: Int -> [Int] -> Int
distance totalTime xs = s * travelTime where
	travelTime = t1 * (ceiling $ n/d)
	s = xs !! 0
	t1 = xs !! 1
	t2 = xs !! 2
	n = fromIntegral totalTime
	d = fromIntegral $ t1+t2    

parseReindeerStats :: String -> [Int]
parseReindeerStats s = map (\x -> read x :: Int) $ getAllTextMatches m where
	m = s =~ "[0-9]+" :: AllTextMatches [] String

reindeerGames :: IO ()
reindeerGames = do
	withFile "adventOfCodeDay14.txt" ReadMode (\handle -> do
		contents <- hGetContents handle
		print $ maximum . map (distance 2503) . map parseReindeerStats . lines $ contents)



-- **** Day 16 ****
-- Aunt Sue
-- part 1 - based on characteristics of 500 Aunt Sue's, figure out which one sent you the gift

parseAunt :: String -> [(String, Integer)]
parseAunt xs = map (\(x,y) -> (x, read y :: Integer)) . map tuplify2 . map cleanString .  splitOn "," $ (tail . dropWhile (/= ':')) $ xs where
	cleanString = (map $ dropWhile (isSpace)) . (\x->splitOn ":" x)

mfcsamAuntSue :: [(String, Integer)]
mfcsamAuntSue = parseAunt $ "Sue 0: cars: 2, akitas: 0, goldfish: 5, children: 3, cats: 7, samoyeds: 2, pomeranians: 3, vizslas: 0, trees: 3, perfumes: 1"

-- part 2 - certain aunt properties are not exact values but ranges. cats and trees are GT values given while pomeranians and goldfish are LT

-- custom subset function based on part 2 constraint i.e. some values are not exact but ranges. We use maps for more convenient subset matching based on ranges
isCustomSubset :: M.Map String Integer -> M.Map String Integer -> Bool
isCustomSubset s1 s2 = all (== True) $ foldr (\x acc -> if (M.lookup x s2) == Nothing then acc else (cmp x): acc) [] (M.keys s1) where
	cmp x
		| x == "cats" || x == "trees" = M.lookup x s1 > M.lookup x s2
		| x == "pomeranians" || x == "goldfish" = M.lookup x s1 < M.lookup x s2
		| otherwise = M.lookup x s1 == M.lookup x s2

auntSue :: IO ()
auntSue = do
	withFile "adventOfCodeDay16.txt" ReadMode (\handle -> do
		contents <- hGetContents handle
		-- part 1 (Add 1 to get final answer)
		print $ length . takeWhile (\x -> not $ x `S.isSubsetOf` (S.fromList mfcsamAuntSue)) . map (S.fromList . parseAunt) . lines $ contents
		-- part 2
		print $ length . takeWhile (\x -> not $ isCustomSubset x (M.fromList mfcsamAuntSue)) . map (M.fromList . parseAunt) . lines $ contents
		) 

-- **** Day 20 ****
tday20 :: Test
tday20 = TestList [ t20part1 ]

-- part 1 : Given number of gifts as input, find lowest house number which gets at least that many gifts
lowestHouseWithGifts :: Int -> Int
lowestHouseWithGifts x = fst . head . filter ((>= x).snd) $ zip [1..] $ map sumOfFactors [1..] where
	sumOfFactors n = foldr (\(a,b) acc -> a+b+acc) 0 [(x, n `div` x)| x <- [1..limit n], n `mod` x == 0]
	limit = floor . sqrt . fromIntegral

t20part1 :: Test
t20part1 = "lowestHouseWithGifts" ~: TestList[lowestHouseWithGifts 8 ~?= 6,
											lowestHouseWithGifts 7 ~?= 4]


-- **** Day 23 ****
-- Mini computer 
-- part 1 - small computer with 2 registers and 6 instruction set. Figure out final output of the registers after given seq. of operations

{-type Instruction = (Integer, String, Integer)

execute :: 

flookup = zip ["inc", "tpl", "hlf", "jmp", "jio", "jie"] [inc, tpl, hlf, jmp, jio, jie]

inc x = x + 1
tpl x = 3 * x
hlf x = x `div` 2
jie x = if x `mod` 2 == 0 then jmp else
jio x = if x == 1 then jmp 


parseInstruction :: String -> (String, Integer)
parseInstruction xs = 

miniComputer :: IO ()
miniComputer = do
	withFile "adventOfCodeDay23.txt" ReadMode (\handle -> do
		contents <- hGetContents handle
		print $ lines contents)
-}




















