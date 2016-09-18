module Main where

import           Criterion
import           Criterion.Main
import           Data.List

-- | First approach, which I think is slower,
-- | but how do I benchmark?
average1 :: (Fractional a) => Int -> [a] -> [a]
average1 n xs = averages ++ [sum ws / fromIntegral count] where
  (count, ws, averages) = foldl' f (0, [], []) xs
  f (_, [], _) e = (1, [e], [])
  f (count, ws@(first:rs), as) e
    | count < n = (count+1, ws++[e], newas)
    | otherwise = (count, rs ++ [e], newas)
    where
      newas = as ++ [sum ws / fromIntegral count]

-- | Second approach - use laziness
movingSum2 :: (Fractional a) => Int -> [a] -> [a]
movingSum2 n xs
  | n <= 1 = xs
  | otherwise = zipWith (+) xs $ movingSum2 (n-1) (0:xs)

average2 :: (Fractional a) => Int -> [a] -> [a]
average2 n xs = zipWith f (movingSum2 n xs) ([1..n] ++ repeat n) where
  f a k = a / fromIntegral k


-- | Third approach - 2n additions
movingSum3 :: (Fractional a) => Int -> [a] -> [a]
movingSum3 n xs
  | n <= 1 = xs
  | otherwise = reverse $ fst $ foldl' f ([], 0) (zip xs $ replicate n 0 ++ xs)
      where
        f (result, acc) (p, m) = let nacc = acc + p - m in (nacc:result, nacc)

average3 :: (Fractional a) => Int -> [a] -> [a]
average3 n xs = zipWith f (movingSum3 n xs) ([1..n] ++ repeat n) where
  f a k = a / fromIntegral k


-- | Artyom's pipeline approach
movingSum4 n xs =
  take (length xs) . map (sum . take n) . tails $ replicate (n-1) 0 ++ xs

average4 :: (Fractional a) => Int -> [a] -> [a]
average4 n xs = zipWith f (movingSum4 n xs) ([1..n] ++ repeat n) where
  f a k = a / fromIntegral k

-- | @thalesmg' refactor of movingSum2
movingSum5 :: (Fractional a) => Int -> [a] -> [a]
movingSum5 w lst =
  let
    shiftedList = zip lst $ replicate w 0 ++ lst
    go _ [] = []
    go acc ((toAdd, toDelete):xs) = (acc + toAdd - toDelete) : go (acc + toAdd - toDelete) xs
  in
    go 0 shiftedList

average5 :: (Fractional a) => Int -> [a] -> [a]
average5 n xs = zipWith f (movingSum5 n xs) ([1..n] ++ repeat n) where
  f a k = a / fromIntegral k

-- main :: IO ()
-- main = do
--   putStrLn
--     "Enter the size of the window, then a list of Ints, all separated by space"
--   xs <- map (read :: String -> Int) . words <$> getLine
--   print $ moving (head xs) (tail $ map fromIntegral xs)

main = defaultMain [
 bench "average1" $nf (average1 100) ([1..1000]::[Double]),
 bench "average2" $ nf (average2 100) ([1..1000]::[Double]),
 bench "average3" $ nf (average3 100) ([1..1000]::[Double]),
 bench "average4" $ nf (average4 100) ([1..1000]::[Double]),
 bench "average5" $ nf (average5 100) ([1..1000]::[Double])]
