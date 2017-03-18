module Reposts where

import System.Random
import Data.Foldable

isWinner :: Int -> [Int] -> Bool
isWinner n = any (<= n)

uniqueSeven :: [Int] -> ([Int], [Int])
uniqueSeven = go [] 7
  where
    go uniques 0 xs = (uniques, xs)
    go uniques _ [] = (uniques, [])
    go uniques n (x:xs)
      | x `elem` uniques = go uniques n xs
      | otherwise        = go (x : uniques) (n - 1) xs

try :: Int -> [Int] -> [Bool]
try n xs = isWinner n numbers : try n rest 
  where
    (numbers, rest) = uniqueSeven xs

probability :: Int -> [Bool] -> Double
probability n = toPercent . sumTrue . take n
  where
    sumTrue :: [Bool] -> Double
    sumTrue = foldl (\acc x -> if x then acc + 1 else acc) 0

    toPercent :: Double -> Double
    toPercent x = (x * 100) / fromIntegral n

main :: IO ()
main = for_ [10, 100, 1000] $ \reposts -> do
    gen <- getStdGen
    let numbers = randomRs (1, 1000000 + reposts) gen
    print $ probability nTries (try reposts numbers)


-- Constants
nTries = 10000

