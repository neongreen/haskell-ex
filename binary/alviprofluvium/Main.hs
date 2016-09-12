import Data.Char
import Numeric.Natural
import Test.QuickCheck

bin :: Natural -> String
bin 0 = "0"
bin n = go n ""
  where
    go :: Natural -> String -> String
    go 0 bs = bs
    go n bs 
      | even n = go (div n 2) $ '0' : bs
      | otherwise = go (div n 2) $ '1' : bs

dec :: String -> Natural
dec = sum . zipWith toPower powersOfTwo . reverse
  where
    toPower :: Natural -> Char -> Natural
    toPower pow dig = pow * toNatural (digitToInt dig)

    powersOfTwo :: [Natural]
    powersOfTwo = iterate (*2) 1

    toNatural :: Int -> Natural
    toNatural = fromInteger . toInteger

procBinary :: Natural -> Property
procBinary n = (dec . bin) n === n

main :: IO()
main = quickCheck procBinary
