{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Random
import Data.Set (Set)
import qualified Data.Set as Set

data Experiment = Experiment {my::Int, other::Int, prizes::Int}

-- returns 0 if we did not win, 1 if we did
draw :: RandomGen g => g -> Experiment -> Int
draw g Experiment{..} = min 1 . hits $ rs where
  hits = Set.size . Set.filter (<= my)
  rs = helper g prizes Set.empty
  helper g 0 xs = xs
  helper g n xs =
    if Set.member r xs
    then helper g'' n xs
    else helper g'' (n-1) (Set.insert r xs)
    where
      (g', g'') = split g
      r = fst $ randomR (1, my + other) g'

repeatDraw :: RandomGen g => g -> Int -> Experiment -> [Int]
repeatDraw g n e = helper g n [] where
  helper g 0 xs = xs
  helper g n xs = helper g'' (n-1) (d:xs) where
    (g', g'') = split g
    d = draw g' e


otherReposts = 1000000
sampleSize = 10000
nrPrizes = 7

myChanceIf :: Int -> IO ()
myChanceIf n = do
  g <- newStdGen
  let
    e = Experiment {my=n, other=otherReposts, prizes=nrPrizes}
    results = repeatDraw g sampleSize e
    probability = fromIntegral (sum results) / fromIntegral sampleSize
  putStrLn $ "For " ++ show n ++ " reposts your chance is " ++ show probability

main :: IO ()
main = do
  myChanceIf 10
  myChanceIf 100
  myChanceIf 1000
