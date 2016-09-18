
average :: [Double] -> Double
average lst = sum lst / fromIntegral (length lst)

moving :: Int -> Int -> [Double] -> [Double]
moving a b = drop (max 0 a) . take b

movingAverage :: Int -> [Double] -> [Double]
movingAverage w lst =
  map (\i -> average $ moving (i - w) i lst) [1..l]
  where
    l = length lst :: Int
    
movingSum :: Int -> [Double] -> [Double]
movingSum w lst =
  let
    shiftedList = zip lst $ replicate w 0 ++ lst
    go :: Double -> [(Double, Double)] -> [Double]
    go _ [] = []
    go acc ((toAdd, toDelete):xs) = (acc + toAdd - toDelete) : go (acc + toAdd - toDelete) xs
  in
    go 0 shiftedList
    
movingAverage2 :: Int -> [Double] -> [Double]
movingAverage2 w lst = 
  zipWith divideBy (movingSum w lst) ([1..w] ++ repeat w)
  where
    divideBy num len = num / fromIntegral len

main :: IO ()
main = do
  let lst = [1,5,3,8,7,9,6]
  print $ movingAverage 4 lst
  print $ movingAverage2 4 lst
