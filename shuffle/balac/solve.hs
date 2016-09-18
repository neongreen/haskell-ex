import Data.Array
import Control.Monad
import Control.Monad.Random
import Graphics.Gnuplot.Simple
import Data.Function
import qualified Data.HashMap.Strict as Map

type MArray = Array Int Int

createArray :: Int -> MArray
createArray n = array (0, n-1) [ (i,i) | i <- [0..n-1] ]

arrayLength :: MArray -> Int
arrayLength = (+1) . snd . bounds 

randomPermute :: (RandomGen g) => MArray -> Int -> Rand g [(Int,Int)]
randomPermute arr pos
    | pos == arrayLen - 1 = return $ assocs arr
    | otherwise = do
        newPos <- getRandomR ( 0, arrayLen - 1 )
        let val = arr ! pos
            newVal = arr ! newPos
        randomPermute ( arr // [ ( pos, newVal ), ( newPos, val ) ] ) ( pos + 1 )
    where
        arrayLen = arrayLength arr

genPermutedArray :: (RandomGen g) => MArray -> Int -> Rand g [[(Int,Int)]]
genPermutedArray arr count = replicateM count ( randomPermute arr 0 )

kArrayLength :: Int
kArrayLength = 50

kNumTrials :: Int
kNumTrials = 100000

createHashTable :: [(Int,Int)] -> Map.HashMap (Int,Int) Int
createHashTable xs = Map.fromListWith (+) ( zip xs ( repeat 1 ) )

probability :: Map.HashMap (Int,Int) Int -> Int -> Int -> Float
probability table x y = ( (/) `on` fromIntegral ) matchCount kNumTrials
    where
        matchCount = Map.lookupDefault 0 (y,x) table

main = do
    permutedArrays <- evalRandIO $ genPermutedArray ( createArray kArrayLength ) kNumTrials
    let assocs = concat permutedArrays
        table  = createHashTable assocs
        plotAttrs = [Plot3dType ColorMap, CornersToColor Corner1]
    plotFunc3d [ZRange (0.014,0.028)] plotAttrs [0..kArrayLength-1] [0..kArrayLength-1] $ probability table
    getLine