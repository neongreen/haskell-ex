import Data.Char
import Data.List

toBinary :: Int -> [Char]
toBinary a = map intToDigit(divisionByTwo a)

divisionByTwo :: Integral a => a -> [a]
divisionByTwo 0 = []
divisionByTwo a = divisionByTwo(a `div` 2) ++ [a `mod` 2]

toInt :: String -> Int
toInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0
