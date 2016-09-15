
average :: [Double] -> Double
average lst = sum lst / fromIntegral (length lst)

moving :: Int -> Int -> [Double] -> [Double]
moving a b = drop (max 0 a) . take b

movingAverage :: Int -> [Double] -> [Double]
movingAverage w lst =
  map (\i -> average $ moving (i - w) i lst) [1..l]
  where
    l = length lst :: Int

main :: IO ()
main = do
  let lst = [1,5,3,8,7,9,6]
  print $ movingAverage 4 lst
