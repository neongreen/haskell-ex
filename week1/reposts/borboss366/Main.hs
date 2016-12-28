module Main where

import Data.Set (Set, size, insert, fromList, lookupIndex, empty, intersection)
import System.Random (RandomGen, mkStdGen, randomR)
import Numeric (showFFloat)

insertIndex :: Int -> Set Int -> Int -> Set Int
insertIndex nTotal st value     | size st >= nTotal = st -- aready filled all the values
                                | otherwise = case ind of
                                                Just _ -> insertIndex nTotal st nextValue
                                                Nothing -> insert value st
                                where   ind = lookupIndex value st
                                        nextValue = (value + 1) `mod` nTotal

insertStep :: RandomGen g => Int -> (Set Int, g) -> (Set Int, g)
insertStep nTotal (st, g) = (insertIndex nTotal st value, g')
                                where (value, g') = randomR (0, nTotal - 1) g

generateDistinct :: RandomGen g => g -> Int -> Int -> (Set Int, g)
generateDistinct g nTotal count | nTotal <= 0 || count <= 0 = (empty, g)
                                | count >= nTotal = (fromList [0..(nTotal - 1)], g)
                                | otherwise = (st, g')
                                where   (st, g') = iterate (insertStep nTotal) (empty, g) !! count

calcSuccessProbability :: RandomGen g => g -> Int -> (g -> (Bool, g)) -> (Double, g)
calcSuccessProbability g n f = (fromIntegral s / fromIntegral n, g')
                                where (s, g') = calcSuccesses g n f

calcSuccesses :: RandomGen g => g -> Int -> (g -> (Bool, g)) -> (Int, g)
calcSuccesses g 0 f = (0, g)
calcSuccesses g n f
        | success = (1 + otherP, g')
        | otherwise = (otherP, g')
        where   (success, newG) = f g
                (otherP, g') = calcSuccesses newG (n - 1) f

makeSimulationSetStep :: RandomGen g => Int -> Int -> Int -> g -> (Bool, g)
makeSimulationSetStep totalPosts winningPosts reposts g = (success, g'')
        where   (winSet, g') = generateDistinct g totalPosts winningPosts
                (repostSet, g'') = generateDistinct g' totalPosts reposts
                intSet = intersection repostSet winSet
                success = size intSet > 0

main :: IO ()
main = do
        print $ "Probability with 10 reposts = " ++ showF prob10
        print $ "Probability with 100 reposts = " ++ showF prob100
        print $ "Probability with 1000 reposts = " ++ showF prob1000
        where   g = mkStdGen 239
                sn = 1000
                f = makeSimulationSetStep 1000000 7
                (prob10, g') = calcSuccessProbability g sn (f 10)
                (prob100, g'') = calcSuccessProbability g' sn (f 100)
                (prob1000, g''') = calcSuccessProbability g'' sn (f 1000)
                showF fval = showFFloat Nothing (fval * 100) "" ++ "%"
