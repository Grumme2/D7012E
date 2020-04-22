module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token, semicolon, becomes,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

cons(a, b) = a:b

(-#) :: Parser a -> Parser b -> Parser b
m -# n = var 
where (_,var) = m#n 

(#-) :: Parser a -> Parser b -> Parser a
m #- n = var
where (var,_) = m#n 

spaces :: Parser String
spaces = space # iter space >-> cons

space :: Parser Char
space = char ? isSpace

token :: Parser a -> Parser a
token m = m #- spaces

letter :: Parser Char
letter = char ? isAlpa

word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String
chars n =  char # chars n-1 >-> cons

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

require :: String -> Parser String
require w 
    | if (==w) = token ( chars (lenght w))
    |otherwise = err 

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

semicolon :: Parser Char
semicolon "; skip" -> Just(';', "skip")
semocolon "skip"-> Nothing


becomes :: Parser String
becomes ":= skip" -> Just(":=", "skip")
becomes "skip" -> Nothing

-- char :: Parser Char
-- Char (c:cs) = Just(c, cs)
-- char[] = Nothing 

-- letter, space :: Parser Char 
