module Main
  where

import Data.Char

scarySum :: String -> Int
scarySum = sum . map (ord' . toLower)
  where
    ord' c = ord c - ord 'a' + 1

isScary :: String -> Bool
isScary = (13 ==) . scarySum . filter isLetter

main :: IO ()
main = getContents
       >>= return . map isScary . words
       >>= print
