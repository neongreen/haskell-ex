module Main where

import Control.Monad (forM_)
import Data.Function (on)
import qualified Data.Sequence as Seq
import Data.Sequence ((<|), Seq(..))

main :: IO ()
main = forM_ [4,2] $ \window -> do
  print $ movingAvgList window [1,5,3,8,7,9,6]

movingAvgList :: Int -> [Int] -> [Float]
movingAvgList windowSize values = map avg $ formWindows windowSize values

formWindows :: Int -> [value] -> [Seq value]
formWindows _ [value] = [Seq.singleton value]
formWindows size (firstValue:otherValues) =
  scanl (\ seq e -> e <| Seq.take (size-1) seq)
        (Seq.singleton firstValue)
        otherValues

avg :: Foldable seq => seq Int -> Float
avg values = sum values /. length values
  where (/.) = (/) `on` fromIntegral
