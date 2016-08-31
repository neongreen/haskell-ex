module Main where

--import Data.Foldable
import Data.Random
import Control.Monad
import Text.Printf

genDiffRandOne :: Int -> [Int] -> RVar Int
genDiffRandOne up rs = do t <- uniform 1 up
                          if t `notElem` rs
                             then return t
                             else genDiffRandOne up rs

growRandList :: Int -> [Int] -> RVar [Int]
growRandList up rs = do t <- genDiffRandOne up rs
                        return (t:rs)

genDiffRandMany :: Int -> Int -> RVar [Int]
-- genDiffRandMany up k = foldlM (\rs _ -> growRandList up rs) [] [1..k]
genDiffRandMany up k = foldr (=<<) (return []) (replicate winners (growRandList up))

trial :: Int -> RVar Bool
trial n = any (<= n) <$> genDiffRandMany (basePosts + n) winners

simulateTrials :: Int -> RVar Float
simulateTrials n = do results <- replicateM trialsNum (trial n)
                      let w = length (filter id results)
                      return (fromIntegral w / fromIntegral trialsNum)

main :: IO ()
main = do w <- sample $ forM [10, 100, 1000] (\x -> simulateTrials x)
          print w

basePosts :: Int
basePosts = 1000000

winners :: Int
winners = 7

trialsNum :: Int
trialsNum = 10000
