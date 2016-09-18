module Average where

average :: [Double] -> Double
average (x:xs) = snd $ foldr (\c (n, avg)-> (n+1, (c + (avg * n)) / (n+1))) (1,x) xs

moving :: Int -> [Double] -> [Double]
moving n xs = foldl (\acc x -> (acc ++ [average (take n $ take x $ drop (x - n) xs)])) [] [1..(length xs)]