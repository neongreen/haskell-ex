{-# LANGUAGE TupleSections #-}


-- General utilities
import Data.Foldable    (for_)
import Data.Traversable (for)
import Data.Tuple       (swap)
import Control.Monad    (replicateM)
-- Arrays
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as MV
import Data.Array.Unboxed
-- Plotting
import Graphics.Gnuplot.Simple
-- Miscellaneous
import System.Random
import System.Environment (getArgs)
import System.IO.Unsafe   (unsafeInterleaveIO)
import Control.Monad.ST   (ST)


{- |
Shuffle an array by swapping pairs:

@
n = array.length
for i in 0..n-1
  array.swap(i, random(0, n-1))
@
-}
shuffleLoop :: Vector a -> IO (Vector a)
shuffleLoop vec = do
  let n = length vec
  -- Generate all swap pairs at once
  rs <- for [0..n-1] $ \i -> (i,) <$> randomRIO (0,n-1)
  -- Apply them all at once with 'modify'
  return (V.modify (swapAll rs) vec)
  where
    swapAll :: [(Int, Int)] -> MV.MVector s a -> ST s ()
    swapAll xs v = for_ xs $ \(i, j) -> MV.swap v i j

-- | Shuffle an array and return a list of positions the elements moved to
positions
  :: (Vector Int -> IO (Vector Int))  -- ^ Shuffle algorithm to use
  -> Int                              -- ^ Length of array to use
  -> IO [(Int, Int)]                  -- ^ (original position, position after)
positions shuffle n =
  map swap . V.toList . V.indexed <$> shuffle (V.enumFromTo 0 (n-1))

-- | Count occurrences of all positions in a 2D array
histogram
  :: Int                       -- ^ Length of shuffled array
  -> [(Int, Int)]              -- ^ Positions in the shuffled array
  -> UArray (Int, Int) Int
histogram n xs = accumArray (+) 0 ((0,0),(n-1,n-1)) (map (,1) xs)

-- | Make a heatmap of a shuffle algorithm
plotShuffle
  :: (Vector Int -> IO (Vector Int))   -- ^ Algorithm
  -> Int                               -- ^ Length of array
  -> Int                               -- ^ How many trials to perform
  -> IO ()
plotShuffle shuffle n kTrials = do
  let trial :: IO [(Int, Int)]
      trial = positions shuffle n
  -- Perform some trials, get a huge list of positions that elements move to
  positions <- concat <$> replicateM kTrials (unsafeInterleaveIO trial)
  -- Count all those positions
  let occurrences = histogram n positions
  -- Calculate the probability that 'x' will jump to 'y'
  let probJump :: Int -> Int -> Double
      probJump x y = fromIntegral (occurrences ! (x,y)) /
                     fromIntegral kTrials
  -- And plot it
  let plotAttrs = [Plot3dType ColorMap, CornersToColor Corner1]
  plotFunc3d [] plotAttrs [0..n-1] [0..n-1] probJump

main = do
  [n, kTrials] <- map read <$> getArgs
  plotShuffle shuffleLoop n kTrials
  getLine
