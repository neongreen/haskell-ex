import System.Random
import Control.Monad.State

nPrizes = 7
totalReposts = 1000000

simulate :: Int -> Int -> State StdGen [Int]
simulate n tr =
  let
    loto :: State StdGen Int
    loto = state $ randomR (1, tr)
    sims :: State StdGen [Int]
    sims = replicateM n loto
  in
    sims

calcProb :: [Int] -> Int -> Double
calcProb sims nps = (fromIntegral . length . filter (<= nps)) sims / (fromIntegral . length $ sims)

-- loto :: State StdGen Int
-- loto = state $ randomR (1, 10)
-- 
-- sims :: State StdGen [Int]
-- sims = replicateM 100 loto

main :: IO ()
main = do
  g <- getStdGen
  let lst = evalState (simulate 10000 totalReposts) g
      prob = calcProb lst nPrizes
  print lst
  print prob
