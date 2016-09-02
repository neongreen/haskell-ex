{-# LANGUAGE RecordWildCards #-}
-- # Reposts

-- Question:
-- seven prizes will be given to seven randomly chosen people among those who
-- have reposted a certain post. There are already ~1,000,000 reposts.
-- My sister wonders: what's the probability of her winning at least one prize
-- (out of those seven) if she reposts the post 10 times
-- (from different accounts)? What about 100 times? 1000 times?

-- Notes:
-- Calculate the answer by running a simulation some number of times
-- (for instance, 10000 times). You can use System.Random or some other
-- random library (e.g. Data.Random).


module Main where

import qualified Data.List as List
import qualified System.Random as Random
import qualified Text.Printf as Print
import qualified Control.Monad as Monad



--  * Pick values for three variables:
--    * a total repost count A
--    * count of winners drawn B
--    * Bob's count of reposts C
--  * Imagine that reposts can be identified by their index in the repost count
--  * Now select winners by randomly generating B indexes in range of A
--  * Now Check how many winners are between 1-C. Yes this assumes that Bob
--    has a contiguous serious of reposts beginning at the head of list A.
--    However! This unlikely detail is inconsequential for the purposes of
--    calculating the probability of Bob being amongst the winners.

type Winners = [Int]
type WinCount = Int

data Experiment = Experiment {
  prizeCount :: Int,
  repostCount :: Int,
  myReposts :: Int
} deriving (Show)



-- Report on several experiments varying by how many times I repost.

main :: IO ()
main = do
  printChanceIfmyReposts 10
  printChanceIfmyReposts 100
  printChanceIfmyReposts 1000
  where
  printChanceIfmyReposts n = do
    let e = Experiment {
      myReposts = n,
      repostCount = 1000000,
      prizeCount = 7
    }
    let probability = myChanceIf e
    Print.printf ("Given "++ show e ++"\nthe probability of winning is %F%%.\n\n") probability



-- Run an experiment many times and then calculate how likely I am to win.

myChanceIf :: Experiment -> Double
myChanceIf experiment = probability
  where

  probability = realToFrac winCount / realToFrac iterations * 100
  winCount = sum . runExperimentTimes experiment $ iterations
  iterations = 10000

  runExperimentTimes :: Experiment -> Int -> [WinCount]
  runExperimentTimes experiment numOfRuns = List.unfoldr go 1
    where
    go runNum
      | runNum > numOfRuns = Nothing
      | otherwise = Just (runExperiment runNum experiment, runNum + 1)



-- Run an experiment showing how many times I win the draw.
-- Note that this is NOT a boolean result since I _can win multiple draws_
-- because I have multiple reposts (obviously excepting the case where I run the
-- experiment with zero or one repost of mine).

runExperiment :: Int -> Experiment -> WinCount
runExperiment seed Experiment{..} =
  countMyWins (generateWinners seed)
  where

  countMyWins :: Winners -> Int
  countMyWins = length . filter (<= myReposts)

  generateWinners :: Int -> Winners
  generateWinners seed =
    take prizeCount .
    List.nub . -- Avoid duplicates. May arise since random...
    Random.randomRs (1, repostCount) $
    generator
    where
    generator = Random.mkStdGen seed



countUnique :: Eq a => [a] -> Int
countUnique = length . List.nub
