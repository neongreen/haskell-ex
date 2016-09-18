module Main where

import Data.Char

bin :: Int -> String
bin n =
  if q == 0
    then show r
    else bin q ++ show r
  where (q,r) = quotRem n 2

dec :: String -> Int
dec xs = sum $ zipWith (*) digits pow
  where digits = reverse $ map digitToInt xs
        pow = iterate (*2) 1

main :: IO ()
main = do
  print $ bin 123
  print $ dec "1111011"
