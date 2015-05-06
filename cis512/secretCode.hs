import Test.HUnit
import Data.Char

{- Replace each letter according to the following correspondence:

            "abcdefghijklmnopqrstuvwxyz"
        to  "thequickbrownfxjmpsvlazydg"

      But leave any non-letter characters alone.

    - Then reverse the order of the lines in the file.
-}

code :: [(Char, Char)]
code = zip ['a'..'z'] cypher ++ 
	zip ['A'..'Z'] (map toUpper cypher) 
	where
		cypher = "thequickbrownfxjmpsvlazydg"

encodeChar :: Char -> Char
encodeChar c = case (lookup c code) of
	Nothing -> c
	Just ch -> ch

encodeLine :: String -> String
encodeLine cs = map encodeChar cs

encodeContent :: String -> String
encodeContent cs = unlines $ map encodeLine (lines cs) 

encodeFile :: FilePath -> IO ()
encodeFile f = do 
	fcontents <- readFile f
	writeFile (f ++ ".code") (encodeContent fcontents)

main :: IO ()
main = do 
	putStrLn "What file shall I encode?"
	fn <- getLine
	encodeFile fn
	putStrLn "All done!"

