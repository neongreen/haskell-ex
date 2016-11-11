module Main where

import Data.Char (isAsciiLower, isAsciiUpper, ord)

main :: IO ()
main = do
  file <- getContents
  let allWords = lines file
  let scaryWords = filter isScary allWords
  mapM_ putStrLn scaryWords

isScary :: String -> Bool
isScary word = sum (map weigh word) == 13
  where weigh char
            | isAsciiLower char = ord char - ord 'a' + 1
            | isAsciiUpper char = ord char - ord 'A' + 1
            | otherwise = 0

