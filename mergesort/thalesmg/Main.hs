import Control.Monad

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [] lstB = lstB
merge lstA [] = lstA
merge lstA@(a:as) lstB@(b:bs) = if a <= b then a : merge as lstB
                                          else b : merge lstA bs
                                          
split :: (Ord a) => [a] -> ([a], [a])
split lst = splitAt (length lst `div` 2) lst

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort lst =
  merge (mergesort lstA) (mergesort lstB)
  where
    (lstA, lstB) = split lst

main :: IO ()
main = do
  xs <- map (read :: String -> Int) . words <$> getContents
  -- print xs
  mapM_ print (mergesort xs)
