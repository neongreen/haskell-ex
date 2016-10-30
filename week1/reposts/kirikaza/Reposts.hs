{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad (forM, forM_, replicateM)
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Random (RVar, runRVar, sampleState, uniform)
import Numeric (showFFloat)
import System.Random (StdGen, getStdRandom)

main :: IO ()
main = forM_ [10, 100, 1000] $ \sisterReposts -> do
  let totalReposts = 1000000
      gifts = 7
      repetitions = 10000
      contest = simContest totalReposts sisterReposts gifts
      repeatContest = repeatSim repetitions contest
  results <- getStdRandom . sampleState $ repeatContest
  let prob = calcWinProb results
      probPercent = prob * 100
      probPercentStr = showFFloat (Just 8) probPercent "%"
  putStrLn $ "if the sister reposts " <> show sisterReposts <> " times, probability for of her winning is " <> probPercentStr
        
repeatSim :: Int -> RVar a -> RVar [a]
repeatSim times sim = replicateM times sim

calcWinProb :: [Bool] -> Double
calcWinProb results = winsNum /. resultsNum
  where winsNum = length $ filter id results
        resultsNum = length results
        (/.) = (/) `on` fromIntegral

simContest :: Int -> Int -> Int -> RVar Bool
simContest totalReposts sisterReposts gifts = do
  giftsNumbers <- selectNumbers 1 totalReposts gifts
  return $ any (<= sisterReposts) giftsNumbers

selectNumbers :: Int -> Int -> Int -> RVar [Int]
selectNumbers min max count = do
  forM [max, max-1 .. max-count+1] $ uniform min    
