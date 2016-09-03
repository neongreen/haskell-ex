-- Scary.hs
-- Assume "words.txt" exists alongside this file

module Scary (scaryWords, isScary) where
import Data.Char (ord)

-- Find all *scary* words in the given list xs
scaryWords :: [String] -> [String]
scaryWords xs = filter isScary xs

isScary :: String -> Bool
isScary s = total == scaryNumber
  where total = foldr (+) 0 $ map charToNumber s
        scaryNumber = 13

charToNumber :: Char -> Int
charToNumber c = (+) asciiOffset $ ord c
  where asciiOffset = -97 + 1

main :: IO()
main = do
  content <- readFile "words.txt"
  print $ scaryWords $ lines content
