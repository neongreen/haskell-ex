import Data.List

mergeSort :: (Ord a) =>  [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort left) (mergeSort right)
  where (left, right) = splitAt half xs
        half = length xs `div` 2

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge a@(x:xs) b@(y:ys)
  | x < y     = x : merge xs b
  | otherwise = y : merge a ys
