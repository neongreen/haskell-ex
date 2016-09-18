import Test.QuickCheck

bin :: Int -> String
bin n =
  let
    (q, r) = divMod n 2
  in
    if q == 0 
      then show r
      else bin q ++ show r
      
      
dec :: String -> Int      
dec b = sum [2^i | (i, '1') <- zip [0..] (reverse b)]
    
identity :: Int -> Bool
identity n = n == (dec $ bin n)
    
main :: IO ()
main = quickCheck (\n -> n >= 0 ==> identity n)
