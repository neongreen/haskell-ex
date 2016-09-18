import Data.Foldable
import Control.Monad
import System.Random
import Data.List
import Text.Printf

-- | Generate 'n_winners' random winners.
randomWinners
  :: Int       -- ^ Total entries
  -> IO [Int]
randomWinners range = do
  ns <- replicateM n_winners (randomRIO (1,range))
  if length (nub ns) < n_winners then randomWinners range else return ns

-- | Simulate one draw by generating some winners and checking whether any of
-- our reposts won.
trial
  :: Int       -- ^ Amount of reposts that are ours
  -> IO Bool
trial reposts = any (<= reposts) <$> randomWinners (n_entries + reposts)

main :: IO ()
main = for_ [10, 100, 1000] $ \reposts -> do
  -- Simulate some trials
  results <- replicateM n_trials (trial reposts)
  -- How many of those were successful?
  let success = length (filter id results)
  -- So, what's the total probability of success?
  let prob = fromIntegral success / fromIntegral n_trials :: Double
  -- Print it (as a percentage)
  printf "for %4d reposts, the probability of winning is %.3f%%\n"
    reposts (prob*100)

-- Constants

n_entries = 1000000
n_winners = 7
n_trials = 10000
