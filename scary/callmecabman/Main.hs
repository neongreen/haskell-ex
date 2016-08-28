module Main
  where

import Data.Char

scarySum :: String -> Int
scarySum = (foldr (+) 0) . (map (ord' . toLower))
  where
    ord' = (+ (1 - ord 'a')) . ord

isScary :: String -> Bool
isScary = ((==) 13) . scarySum . (filter isLetter)

main :: IO ()
main = do getContents
          >>= return . (map isScary) . words
          >>= print
