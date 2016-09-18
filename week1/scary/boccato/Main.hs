-- stack runghc Main.hs < /usr/share/dict/words

module Main where

import Data.Char

numberOf :: Char -> Int
numberOf c
  | isLetter c = ord (toLower c) - ord 'a' + 1
  | otherwise  = 0

isScary :: String -> Bool
isScary w = sum (map numberOf w) == 13

main :: IO ()
main = do
  ws <- lines <$> getContents
  mapM_ putStrLn $ filter isScary ws
