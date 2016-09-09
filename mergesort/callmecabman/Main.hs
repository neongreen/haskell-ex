module Main where

import Data.Bifunctor

split :: [a] -> ([a], [a])
split = split' [] []
  where
    split' xs ys []     = (xs, ys)
    split' xs ys (z:zs) = split' ys (z:xs) zs

merge :: Ord a => ([a], [a]) -> [a]
merge ([], ys) = ys
merge (xs, []) = xs
merge (s@(x:xs), t@(y:ys))
  | x <= y     = x : merge (xs, t)
  | otherwise = y : merge (s, ys)

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge $ bimap mergeSort mergeSort $ split xs

main :: IO ()
main = return ()
