import Data.List

moving :: Int -> [Double] -> [Double]
moving _ [] = []
moving n list 
  | n <= 0    = list
  |otherwise  = map average movingList
  where
    average :: [Double] -> Double
    average xs = sum xs / genericLength xs

    movingList :: [[Double]]
    movingList = map move $ tail $ inits list
      where
        move :: [Double] -> [Double]
        move x = if length x > n then drop (length x - n) x else x
