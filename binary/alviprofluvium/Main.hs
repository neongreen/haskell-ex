import Data.Char
import Numeric.Natural
import Test.QuickCheck

bin :: Natural -> String
bin 0 = "0"
bin n = go n ""
  where
    go :: Natural -> String -> String
    go 0 bs = bs
    go n bs = go half $ (intToDigit . fromEnum) rest : bs
      where
        (half, rest) = divMod n 2

dec :: String -> Natural
dec = sum . zipWith toPower powersOfTwo . reverse
  where
    toPower :: Natural -> Char -> Natural
    toPower pow dig = pow * toEnum (digitToInt dig)

    powersOfTwo :: [Natural]
    powersOfTwo = iterate (*2) 1

procBinary :: Natural -> Property
procBinary n = (dec . bin) n === n

main :: IO()
main = quickCheck procBinary
