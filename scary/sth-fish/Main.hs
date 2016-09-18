import System.IO
import Data.Char

abcOrd :: Char -> Int
abcOrd char
  | isAscii char && isLetter char = ord (toLower char) - ord 'a' + 1
  | otherwise = 0

abcOrdSum :: String -> Int
abcOrdSum word = sum (map abcOrd word)

isScary :: String -> Bool
isScary word = abcOrdSum word == 13

main = do
  contents <- readFile "words"
  let words = lines contents
  print(filter isScary words)
