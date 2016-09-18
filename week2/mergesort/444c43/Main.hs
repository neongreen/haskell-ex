mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort all@(x:xs) =
    let (left, right) = splitlist all
    in merge (mergesort left) (mergesort right)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge left@(x:xs) right@(y:ys)
  | x <= y = x : merge xs right
  | y <= x = y : merge left ys

splitlist :: [a] -> ([a], [a])
splitlist list = splitAt ((length list + 1) `div` 2) list
