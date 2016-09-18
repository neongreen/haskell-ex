module Main where

import Data.Char

scaryInt = 13

isGoodChar :: Char -> Bool
isGoodChar c = isAscii c && isLetter c

scaryCharToInt :: Char -> Int
scaryCharToInt c
  | isGoodChar c = ord (toLower c) - 96
  | otherwise = 0

scarySum :: String -> Int
scarySum = helper 0 where
  helper :: Int -> String -> Int
  helper acc [] = acc
  helper acc (c:cs)
    | acc > scaryInt = acc
    | otherwise = helper (acc + scaryCharToInt c) cs


isScary :: String -> Bool
isScary cs = scaryInt == scarySum cs

main :: IO ()
main = do
  dict <- getContents
  let scaryWords = filter isScary (lines dict)
  putStrLn $ unlines scaryWords
