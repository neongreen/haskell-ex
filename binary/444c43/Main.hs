import Data.Char

toBinary :: Int -> [Char]
toBinary a = map intToDigit(convertDecimal a)

convertDecimal :: Integral a => a -> [a]
convertDecimal 0 = []
convertDecimal a = convertDecimal(a `div` 2) ++ [a `mod` 2]
