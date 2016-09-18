module Binary where

import Data.Char

bin :: Int -> String
bin 0 = ['0']
bin 1 = ['1']
bin n = bin (n `div` 2) ++ bin (n `mod` 2)

dec :: String -> Int
dec = foldl (\acc x -> acc * 2 + (digitToInt x)) 0