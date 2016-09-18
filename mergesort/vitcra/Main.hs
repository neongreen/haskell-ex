module Main where

import           System.Random

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort zs = merge xs ys where
  h = div (length zs) 2
  xs = mergesort $ take h zs
  ys = mergesort $ drop h zs
  merge [] bs = bs
  merge as [] = as
  merge ls@(a:as) hs@(b:bs) = if a <= b then a:merge as hs else b:merge ls bs

main :: IO ()
main = do
  g <- newStdGen
  let xs = take 10 $ randomRs (0::Int, 20) g
  print xs
  putStrLn "Now mergesorted:"
  print $ mergesort xs
