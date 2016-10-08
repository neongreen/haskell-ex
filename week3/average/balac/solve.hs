import Data.Function

sampleList = [1,5,3,8,7,9,6]

moving :: Int -> [Integer] -> [Double]
moving winSize list = zipWith ( (/) `on` fromInteger ) sums winSizes
    where
        deltas  = zipWith (-) list $ replicate winSize 0 ++ list
        sums    = scanl1 (+) deltas
        winSizes= map toInteger ( [1..winSize] ++ repeat winSize )

main :: IO ()
main = print $ moving 4 sampleList