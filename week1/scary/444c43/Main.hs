import System.IO
import Data.Char
import Data.List


sumString :: String -> Int
sumString []     = 0
sumString (x:xs) =
  let offset = fromEnum (toLower x) - 96
  in offset + sumString xs

sumList :: [String] -> Int
sumList []     = 0
sumList (x:xs) = sumString x + sumList xs

main = do
  contents <- readFile "/usr/share/dict/words"
  print . sumList . lines $ contents
