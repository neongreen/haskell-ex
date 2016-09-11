merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | y <= x = y : merge (x:xs) ys

splitlist :: [a] -> ([a], [a])
splitlist list = splitAt ((length list + 1) `div` 2) list
