import Data.List
import Data.Bits
import Data.Char

bin :: Int -> String
bin 0 = "0"
bin n = reverse $ unfoldr (\x -> if x == 0 then Nothing else Just ( intToDigit ( x .&. 1 ), shift x (-1) ) ) n

dec :: String -> Int
dec = foldl' ( \acc char -> ( shift acc 1 ) + digitToInt char ) 0