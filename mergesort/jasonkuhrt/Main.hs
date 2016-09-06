
--  # Mergesort

--  Implement a mergesort
--  Invented by John von Neumann in 1945
--  Reference: https://en.wikipedia.org/wiki/Merge_sort

--  * Split the list into two sublists (in any way).
--  * Recursively sort the two sublists.
--  * Merge the sublists in such a way that if they are sorted,
--    the result is sorted too.



module Main where

main :: IO ()
main = demo

demo :: IO ()
demo = do
  print unsortedNumbers
  putStr "==> MERGESORT ==>\n"
  print (mergesort unsortedNumbers) where
  unsortedNumbers :: [Int]
  unsortedNumbers = [1,12,5,6,21,2,61,126,7,87,23,1,451,16,46,35]



--  Assmeble our merge sort

mergesort :: Ord a => [a] -> [a]
mergesort = merge . sortTwo . split



--  Split the list into two even halves.
--  For odd-length lists the *second half is one-item longer*.

split :: [a] -> ([a],[a])
split list = splitAt middleIndex list where
  middleIndex = div (length list) 2



--  Recursively sort the two sublists.

sortTwo :: Ord a => ([a],[a]) -> ([a],[a])
sortTwo (os,xs) = (sortOne os, sortOne xs) where
-- Not using a fold because we need _two_ items per iteration
  sortOne []  = []
  sortOne [v] = [v]
  sortOne (v:x:zs)
    | v > x     = xFirst
    | v < x     = vFirst
    | otherwise = vFirst -- Preserve input order
    where
    xFirst = x : sortOne (v:zs)
    vFirst = v : sortOne (x:zs)

-- NOTE Alternative Implementations
-- sortTwo (xs,zs) = (List.sort xs, List.sort zs)
-- sortTwo = uncurry (Func.on (,) List.sort)



--  Merge the two sublists such that if they are sorted
--  the result is sorted too.

merge :: Ord a => ([a],[a]) -> [a]
merge (xs,[])         = xs
merge ([],zs)         = zs
merge (xxs@(x:xs),zzs@(z:zs))
  | x > z     = zFirst
  | x < z     = xFirst
  | otherwise = xFirst -- Preserve input order
  where
  zFirst = z : merge (xxs,zs)
  xFirst = x : merge (xs,zzs)
