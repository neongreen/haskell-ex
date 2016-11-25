merge :: (Ord a) => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (x:a) (y:b)
    | x < y = x : merge a (y:b)
    | otherwise = y : merge (x:a) b

topDown :: (Ord a) => [a] -> [a]
topDown a
    | null a        = []
    | length a == 1 = a
    | otherwise =
        let index = length a `div` 2
        in
            merge (topDown $ take index a) (topDown $ drop index a)

downUp :: (Ord a) => [a] -> [a]
downUp a = concat $ downUpImpl [[x] | x <- a]
    where
        downUpImpl :: (Ord a) => [[a]] -> [[a]]
        downUpImpl [] = []
        downUpImpl [a]  = [a]
        downUpImpl (x:y:xs) = downUpImpl $ merge x y : downUpImpl xs
