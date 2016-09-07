import Data.Array
import Control.Monad
import Control.Monad.Random
import Graphics.Gnuplot.Simple
import Data.Function

type MArray = Array Int Int

createArray :: Int -> MArray
createArray n = array (0, n-1) [ (i,i) | i <- [0..n-1] ]

arrayLength :: MArray -> Int
arrayLength = (+1) . snd . bounds 

randomPermute :: (RandomGen g) => MArray -> Int -> Rand g MArray
randomPermute arr pos
    | pos == arrayLen - 1 = return arr
    | otherwise = do
        newPos <- getRandomR ( 0, arrayLen - 1 )
        let val = arr ! pos
        let newVal = arr ! newPos
        randomPermute ( arr // [ ( pos, newVal ), ( newPos, val ) ] ) ( pos + 1 )
    where
        arrayLen = arrayLength arr

genPermutedArray :: (RandomGen g) => MArray -> Int -> Rand g [MArray]
genPermutedArray arr count = replicateM count ( randomPermute arr 0 )

kArrayLength :: Int
kArrayLength = 50

kNumTrials :: Int
kNumTrials = 100000

probability :: [MArray] -> Int -> Int -> Float
probability table x y = ((/) `on` fromIntegral) matchCount ( length table )
    where
        matchCount = length $ filter (==x ) $ map ( ! y ) table

main = do
    permutedTable <- evalRandIO $ genPermutedArray ( createArray kArrayLength ) kNumTrials
    let plotAttrs = [Plot3dType ColorMap, CornersToColor Corner1]
    plotFunc3d [] plotAttrs [0..kArrayLength-1] [0..kArrayLength-1] $
        \x y -> probability permutedTable x y
    getLine