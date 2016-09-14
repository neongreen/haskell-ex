import Test.QuickCheck

bin :: Int -> String
bin n =
  let
    findPower m last = if 2^(last+1) > m then last else findPower m (last + 1)
    maxPower m = findPower m 0
    powers m = if m <= 0 then [] else maxPower m : powers (m - 2^(maxPower m))
    lst = powers n
    topOne = head lst
    go i []     = if i < 0 then [] else '0' : go (i-1) []
    go i (p:ps) = if i == p then '1' : go (i-1)    ps 
                  else           '0' : go (i-1) (p:ps) 
  in
    if lst == [] then "0"
    else
      '1' : go (topOne - 1) (tail lst)
      
dec :: String -> Int      
dec b =
  let
    lst = zip (reverse b) [0..]
  in
    sum . map (\(bit, power) -> if bit == '1' then 2^power else 0) $ lst
    
identity :: Int -> Bool
identity n = n == (dec $ bin n)
    
main :: IO ()
main = quickCheck (\n -> n >= 0 ==> identity n)
