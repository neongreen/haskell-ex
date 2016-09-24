import Data.Foldable (for_)

{-
Take a spiral with e.g. size 9 and look at the empty space inside:

    *********
            *       xxxxxxxx
    ******* *              x
    *     * *        xxxxx x
    * *** * *        x   x x
    * * * * *        x x x x
    * *   * *        x xxx x
    * ***** *        x     x
    *       *        xxxxxxx
    *********

Apart from the extra space in the beginning, it's a spiral with size 7.
-}

inSpiral :: Int -> (Int, Int) -> Bool
inSpiral n (x,y) = or [
  -- First line, last line, and last column are always filled
  y == 0, y == n, x == n-1,
  -- Only one element in the first column isn't filled
  x == 0 && y /= 1,
  -- Otherwise, the filled elements are ones that wouldn't be filled in a
  -- smaller spiral
  not (inSpiral (n-2) (x-1, y-1)) ]

main = do
  putStr "Size? "
  n <- readLn
  for_ [0..n] $ \y -> do
    for_ [0..n-1] $ \x ->
      putChar (if inSpiral n (x,y) then '*' else ' ')
    putStrLn ""
