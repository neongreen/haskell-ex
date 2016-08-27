import System.IO
import Data.Char

intVal :: Char -> Int
intVal c = ord ( toLower c ) - 96

isScary :: [Char] -> Bool
isScary word = ( sum $ map intVal $ filter isAsciiLower word ) == 13

main = do
    contents <- readFile "words.txt"
    let words = lines contents
    let scaryWords = filter isScary words
    print scaryWords
