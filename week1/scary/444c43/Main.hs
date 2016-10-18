import System.IO
import Data.Char
import Data.List

-- given a string
--   convert the head to a number, if it's a letter
--   subtract the offset and add the value of tail passed to self
sumString :: String -> Int
sumString []                    = 0
sumString (x:xs)
  | (x `elem` letters) == True  = (fromEnum (toLower x) - offset) + sumString xs
  | (x `elem` letters) == False = 0 + sumString xs
  where letters = ['a'..'z'] ++ ['A'..'Z']
        offset  = 96

-- create a new list with each element of a list whose summed total is 13
parseList :: [String] -> [String]
parseList xs = [ x | x <- xs, sumString x == 13]

-- read from a file, conver to lines, pass to sumList and print to screen
main = do
  contents <- readFile "/usr/share/dict/words"
  print . sumList . lines $ contents
