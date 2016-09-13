module Main where

import           Data.List

-- | First approach, which I think is slower,
-- | but how do I benchmark?
-- moving :: (Fractional a) => Int -> [a] -> [a]
-- moving n xs = averages ++ [sum ws / fromIntegral count] where
--   (count, ws, averages) = foldl' f (0, [], []) xs
--   f (_, [], _) e = (1, [e], [])
--   f (count, ws@(first:rs), as) e
--     | count < n = (count+1, ws++[e], newas)
--     | otherwise = (count, rs ++ [e], newas)
--     where
--       newas = as ++ [sum ws / fromIntegral count]

-- | Second approach - use laziness
movingSum :: (Fractional a) => Int -> [a] -> [a]
movingSum n xs
  | n <= 1 = xs
  | otherwise = zipWith (+) xs $ movingSum (n-1) (0:xs)

moving :: (Fractional a) => Int -> [a] -> [a]
moving n xs = map f sums where
  sums = zip (movingSum n xs) ([1..n] ++ repeat n)
  f (a, k) = a / fromIntegral k

main :: IO ()
main = do
  putStrLn
    "Enter the size of the window, then a list of Ints, all separated by space"
  xs <- map (read :: String -> Int) . words <$> getLine
  print $ moving (head xs) (tail $ map fromIntegral xs)
