import System.IO
import Data.Char

intVal :: Char -> Int
intVal c = ord c - 96

isScary :: [Char] -> Bool
isScary word = ( sum $ map intVal word ) == 13

main = do
    contents <- readFile "words.txt"
    let words = lines $ map toLower contents
    let scaryWords = filter isScary words
    putStr $ show scaryWords
