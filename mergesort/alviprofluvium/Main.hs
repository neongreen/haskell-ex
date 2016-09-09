import Data.List

mergeSort :: (Ord a) =>  [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort left) (mergeSort right)
  where (left, right) = divide xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge a@(x:xs) b@(y:ys)
  | x <= y     = x : merge xs b
  | otherwise  = y : merge a ys

divide :: [a] -> ([a],[a])
divide = go [] []
  where go as bs []     = (as, bs)
        go as bs (x:xs) = go bs (x:as) xs
