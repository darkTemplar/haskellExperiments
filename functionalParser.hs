module Parsing where

import Data.Char
import Control.Monad

-- type ST a = State -> [(a, State)]
newtype Parser a = P(String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P f) x = f x

instance Monad Parser where
	-- return :: a -> Parser a
	return v = P(\ inp -> [(v, inp)])  

	-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
	p >>= f = P(\ inp -> case parse p inp of
		[] -> []
		[(v, out)] -> parse (f v) out)


failure :: Parser a
failure = P (\ inp -> [])

item :: Parser Char
item = P (\ inp -> case inp of
	[] -> []
	(x:xs) -> [(x,xs)])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P(\ inp -> case parse p inp of
	[] -> parse q inp
	[(v, out)] -> [(v, out)])

-- Other useful parsing primitives
sat :: (Char -> Bool) -> Parser Char
sat p =  do x <- item
            if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
	char x
	string xs
	return (x:xs)


alphanum :: Parser Char
alphanum = sat isAlphaNum

alpha :: Parser Char
alpha = sat isAlpha

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

-- multiple applications of Parsers
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v  <- p
             vs <- many p
             return (v:vs)

-- Parser for identifier
ident :: Parser String
ident = do 
	x  <- lower
	xs <- many alphanum
	return (x:xs)

-- Parser for spaces
space :: Parser ()
space = do
	many (sat isSpace)
	return ()

-- Parser for natural numbers
nat :: Parser Int
nat = do
	xs <- many digit
	return (read xs)

-- Parser for literal Int (integers with optional '-' in front)
literalInt :: Parser Int
literalInt = (do
	char '-'
	n <- nat
	return (-n)) +++ nat

-- parse token while ignoring space before it
token :: Parser a -> Parser a
token p = do
	space
	v <- p
	return v

-- parser for comments
comment :: Parser ()
comment = do
	string "--"
	many (sat (/= '\n'))
	return ()


			



