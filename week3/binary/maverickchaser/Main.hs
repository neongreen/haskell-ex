bin :: Int -> String
bin 0 = ""
bin x = bin (x `div` 2) ++ show (x `mod` 2)

enumerate = zip [0..]

dec :: String -> Integer
dec = sum . map toAdd . enumerate . reverse
    where set x
             | x == '1' = 1
             | x == '0' = 0
          toAdd (power,bit) = 2^power * set bit
