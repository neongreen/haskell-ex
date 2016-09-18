-- Scary.hs
-- Assume "words.txt" exists alongside this file

module Scary (scaryWords, isScary) where
import Data.Char (ord, toLower)

-- Find all *scary* words in the given list xs
scaryWords :: [String] -> [String]
scaryWords xs = filter isScary xs

isScary :: String -> Bool
isScary s = total == scaryNumber
  where total = sum $ map charToNumber s
        scaryNumber = 13

charToNumber :: Char -> Int
charToNumber c
  | elem char alphabet = (+) asciiOffset $ ord char
  | otherwise = 0
  where asciiOffset = -97 + 1
        alphabet = ['a'..'z']
        char = toLower c

main :: IO()
main = do
  content <- readFile "words.txt"
  print $ scaryWords $ lines content
