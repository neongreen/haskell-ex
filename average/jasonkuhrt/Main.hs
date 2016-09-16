


main :: IO ()
main = do
  print $ smm 4 exampleSet
  print $ smm 4 exampleSet == [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
  print $ smm 2 exampleSet == [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
  where
  exampleSet = [1,5,3,8,7,9,6]


{- | Calculate the simple moving mean.

See an introduction about moving means at
https://en.wikipedia.org/wiki/Moving_average -}
smm :: Int -> [Float] -> [Float]
smm windowSize dataPoints                      -- Sensible Guards:
  | windowSize == 1 = dataPoints               -- means of size 1 are ids
  | windowSize == 0 = map (const 0) dataPoints -- means of size 0 are 0s
  | windowSize <= 0 = []                       -- nonsense
  | otherwise       = go 1 dataPoints
  where
  go n xs
    -- Stop when the shifting data shrinks smaller than window
    -- If window is immature then do grow but no shift
    -- If window is mature then no grow but do shift
    | n > length xs  = []
    | n < windowSize = mean (take n xs) : go (n + 1) xs
    | otherwise      = mean (take n xs) : go n (drop 1 xs)


{- | Calculate the mean value of a set. -}
mean :: [Float] -> Float
mean [] = 0
mean xs = sum xs / (realToFrac . length) xs
