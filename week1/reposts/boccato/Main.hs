module Main where

import Control.Monad
import Data.List
import System.Random
import Text.Printf

nWinners = 7
nSimulations = 10000
nCurrentReposts = 1000000

takeDistinct :: Eq a => Int -> [a] -> [a]
takeDistinct n xs = sample n xs []
  where
    sample _ []     res  = res
    sample n (x:xs) res
      | n == 0       = res
      | x `elem` res = sample n xs res
      | otherwise    = sample (n-1) xs (x:res)

-- nWinners are taken from a pool of n+nCurrentReposts tickets
simulate :: Int -> IO Bool
simulate n = do
  g <- newStdGen
  let nReposts = n + nCurrentReposts
  let xs = randomRs(1,nReposts) g
  return $ any (<= n) (takeDistinct nWinners xs)

probability :: Int -> IO Float
probability reposts = do
  xs <- replicateM nSimulations $ simulate reposts
  let wins = length $ filter (== True) xs
  return $ fromIntegral wins / fromIntegral nSimulations * 100

main :: IO ()
main = forM_ [10, 100, 1000, 10000, 100000, 1000000] $ \reposts -> do
  p <- probability reposts
  printf "Probability of %5.2f%% for %d reposts.\n" p reposts
