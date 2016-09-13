import Data.Char

toBinary :: Int -> [Char]
toBinary a = map intToDigit(convertDecimal a)

divisionByTwo :: Integral a => a -> [a]
divisionByTwo 0 = []
divisionByTwo a = convertDecimal(a `div` 2) ++ [a `mod` 2]
