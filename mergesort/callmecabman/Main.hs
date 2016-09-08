module Main where

import Data.Bifunctor

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge s@(x:xs) t@(y:ys)
  | x <= y = x : merge xs t
  | x > y = y : merge s ys

mergeSort' :: Ord a => Int -> [a] -> [a]
mergeSort' n xs
  | n <= 1 = xs
  | n > 1 = uncurry merge $ mapSort (splitAt k xs)
    where
      mapSort = bimap (mergeSort' k) (mergeSort' (n - k))
      k = n `div` 2

mergeSort :: Ord a => [a] -> [a]
mergeSort xs = mergeSort' (length xs) xs

main :: IO ()
main = return ()
