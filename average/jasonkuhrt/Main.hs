{- # Moving Mean

See an introduction about moving means at
https://en.wikipedia.org/wiki/Moving_average
-}

main :: IO ()
main = do
  print $ smm 4 exampleSet
  print $ smm 4 exampleSet == [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
  print $ smm 2 exampleSet == [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
  where
  exampleSet = [1,5,3,8,7,9,6]


{- | Calculate the simple moving mean. -}
smm :: Int -> [Float] -> [Float]
smm = growMove mean


{- | Calculate the mean value of a set. -}
mean :: [Float] -> Float
mean [] = 0
mean xs = sum xs / (realToFrac . length) xs


{- | Repeatedly apply f to subsets taken from a moving window.

The window must first mature before sliding. For example:

    growMove f 4 [1,5,3,8,7,9,6]

    -- Below target size. Grow window by 1 for each step
    f [1]         = a
    f [1,5]       = b
    f [1,5,3]     = c
    -- At/above target size. Now repeatedly drop 1 for each step
    f [1,5,3,8]   = d
    f [5,3,8,7]   = e
    f [3,8,7,9]   = f
    f [8,7,9,6]   = g

    returns [a,b,c,d,e,f,g]
-}
growMove :: ([a] -> a) -> Int -> [a] -> [a]
growMove f windowSize xs
  | windowSize <= 0 = []
  | otherwise       = go 1 xs
  where
  go size rest
    | size > length rest = []
    | size < windowSize  = f (take size rest) : go (size + 1) rest
    | otherwise          = f (take size rest) : go size (drop 1 rest)
