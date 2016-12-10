import Data.Char
import Data.List

scareFactor :: Char -> Int
scareFactor c 
    | isLetter c = ord (toUpper c) - ord 'A' + 1
    | otherwise  = 0

checkWord :: [Char] -> Int
checkWord = sum . map scareFactor

main = do
    dictionary <- readFile "/usr/share/dict/words"
    let wordsToCheck = lines dictionary
    let scaryWords = [x | x <- wordsToCheck, (checkWord x) == 13]
    print(scaryWords)