import Data.Char
import Data.List

bin :: Integer -> String
bin 0 = "0"
bin n = map (intToDigit . fromIntegral) . reverse . toDigits $ n
  where
    toDigits 0 = []
    toDigits n = n `mod` 2 : toDigits (n `div` 2)

dec :: String -> Integer
dec = foldl' (\n digit -> n*2 + fromIntegral (digitToInt digit)) 0
