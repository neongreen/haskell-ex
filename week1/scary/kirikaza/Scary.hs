module Main where

import Data.Char (isAsciiLower, isAsciiUpper, ord)

main :: IO ()
main = do
  file <- getContents
  let words = lines file
  let scaryWords = filter isScary words
  mapM_ putStrLn scaryWords

isScary :: String -> Bool
isScary word = foldl plusLetter 0 word == 13
  where plusLetter sum char = sum + weigh char
        weigh char
            | isAsciiLower char = ord char - ord 'a' + 1
            | isAsciiUpper char = ord char - ord 'A' + 1
            | otherwise = 0

