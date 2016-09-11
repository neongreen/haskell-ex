mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort all@(x:xs) =
    let left  = fst(splitlist all)
        right = snd(splitlist all)
    in merge (mergesort left) (mergesort right)

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | y <= x = y : merge (x:xs) ys

splitlist :: [a] -> ([a], [a])
splitlist list = splitAt ((length list + 1) `div` 2) list
