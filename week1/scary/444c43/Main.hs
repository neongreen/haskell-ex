import System.IO
import Data.Char
import Data.List

sumString :: String -> Int
sumString []                    = 0
sumString (x:xs)
  | (x `elem` letters) == True  = (fromEnum (toLower x) - offset) + sumString xs
  | (x `elem` letters) == False = 0 + sumString xs
  where letters = ['a'..'z'] ++ ['A'..'Z']
        offset  = 96

sumList :: [String] -> [String]
sumList xs = [ x | x <- xs, sumString x == 13]

main = do
  contents <- readFile "/usr/share/dict/words"
  print . sumList . lines $ contents
