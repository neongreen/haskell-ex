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

main :: IO ()
main = simulate



simulate :: IO ()
simulate = do
  let winCount = realToFrac $ countWinsPerTrials trialCount :: Double
  let winProbability = winCount / realToFrac trialCount * 100
  Print.printf "Probability of Bob winning is %F%%." winProbability

countWinsPerTrials :: Int -> Int
countWinsPerTrials numOfRuns = sum (List.unfoldr go 1)
  where
  go runNum
    | runNum > numOfRuns = Nothing
    | otherwise = Just (runTrial runNum, runNum + 1)

runTrial :: Int -> Int
runTrial = countBobsWins . generateWinners

countBobsWins :: Winners -> Int
countBobsWins = length . filter (<= bobsRepostCount)

generateWinners :: Int -> Winners
generateWinners seed =
  take winnersDrawn $ Random.randomRs (1, totalRepostCount) generator
  where
  generator = Random.mkStdGen seed



-- Variables --

-- Examples:
-- 7 | 1 mil | 10   = .01%
-- 7 | 1 mil | 100  = .06%
-- 7 | 1 mil | 1000 = .74%

winnersDrawn = 7
totalRepostCount = 1000000
bobsRepostCount = 1000
trialCount = 10000
