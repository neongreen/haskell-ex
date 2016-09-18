module Main where

bin :: Int -> String
bin n = bits where
  bits = loop n []
  loop x acc = if next == 0 then d:acc else loop next (d:acc)
    where
      (next, r) = divMod x 2
      d = convert r
      convert 1 = '1'
      convert _ = '0'

dec :: String -> Int
dec s = foldr f 0 (zip ds [0..]) where
  ds = map convert (reverse s)
  convert '1' = 1
  convert _ = 0
  f (d, i) acc = acc + d*2^i

main :: IO ()
main = undefined
