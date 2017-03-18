import System.Random
import Control.Monad.State

nPrizes = 7
totalReposts = 1000000
nSims = 10000

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
calcProb sims nSisterPosts = (fromIntegral . length . filter (<= nSisterPosts)) sims / (fromIntegral . length $ sims)

-- loto :: State StdGen Int
-- loto = state $ randomR (1, 10)
-- 
-- sims :: State StdGen [Int]
-- sims = replicateM 100 loto

main :: IO ()
main = do
  g <- getStdGen
  let sim nSister = evalState (simulate nSims (totalReposts + nSister)) g
      extras = [10, 100, 1000]
      lists = map sim extras
      probs = zipWith calcProb lists extras
  -- putStrLn . show $ zip extras probs
  mapM_ (\(sis, prob) -> putStrLn $ "With " ++ show sis ++ " reposts, the probability "
         ++ "is " ++ show prob) (zip extras probs)
