module Main where

import           Data.Char

getCode :: Char -> Int
getCode ch =
  let
    zeroIndex = ord 'a' - 1
  in
    (ord . toLower $ ch) - zeroIndex

isScary :: String -> Bool
isScary [] = False
isScary s =
   sum (map getCode s) == 13

main :: IO()
main = do
 contents <- words <$> readFile "C:\\temp\\words.txt"
 putStrLn $ unwords (map addNewLine (filter isScary contents))
 where
   addNewLine :: String -> String
   addNewLine s = s ++ "\n"
