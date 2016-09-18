
-- # Mergesort

-- Implement a mergesort
-- Invented by John von Neumann in 1945
-- Reference: https://en.wikipedia.org/wiki/Merge_sort



module Main where

-- ## Demo

main :: IO ()
main = do
  print unsortedNumbers
  putStr "==> MERGESORT ==>\n"
  print (mergesort unsortedNumbers) where
  unsortedNumbers :: [Int]
  unsortedNumbers = [1,12,5,6,21,2,61,126,7,87,23,1,451,16,46,35]



-- ## Program

mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs  = go (mergesort xsLeft) (mergesort xsRight) where
  (xsLeft, xsRight) = halveList xs

  -- Do the actual sort-then-merge

  go :: Ord a => [a] -> [a] -> [a]
  go xs [] = xs
  go [] zs = zs
  go xxs@(x:xs) zzs@(z:zs)
    | x > z         = zFirst
    | x < z         = xFirst
    | otherwise     = xFirst -- Preserve order AKA "stable sort"
    where
    zFirst = z : go xxs zs
    xFirst = x : go xs zzs



-- ## Helpers

halveList :: [a] -> ([a],[a])
halveList xs = splitAt (div (length xs) 2) xs
