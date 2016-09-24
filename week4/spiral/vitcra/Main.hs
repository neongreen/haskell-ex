module Main where

import           Data.List
import           Data.Set  (Set)
import qualified Data.Set  as S

-- | Returns the graph set of the spiral,
-- | i.e the points that are on the spiral.
spiralGraph :: Int -> Set (Int, Int)
spiralGraph n = fst $ foldl' f (S.empty, (0,-1)) $ zip counts deltas
      -- | The start is outside the grid  ^^^^
      -- | We'll step into (0,0) right away.
  where
    -- | Counts are basically the lengths of the lines of the spiral
    -- | Here's the pattern: (n: pattern)
    -- | 9: 9 8 8 6 6 4 4 2 2
    -- | 8: 8 7 7 5 5 3 3 1 1
    -- | 7: 7 6 6 4 4 2 2
    -- | 6: 6 5 5 3 3 1 1
    -- | etc
    counts = n : concat [[n - 2*i - 1, n - 2*i - 1] | i <- [0..div n 2 - 1]]
    -- | Go right, down, left, up and then cycle that.
    deltas = cycle [(0,1), (1, 0), (0, -1), (-1, 0)]
    -- | So zip counts deltas is like:
    -- | go 9 times right, 8 times down, 8 times left, 6 times up, etc
    -- | Put the visited points into the Graph set.

    -- | Helper function that add two pairs
    add (a, b) (c, d) = (a+c, b+d)
    -- | The fold function that carries current position along with the Graph.
    f (set, current) (count, delta) = foldl' g (set, current) [1..count] where
      g (s, c) _ = let next = add c delta in (S.insert next s, next)

-- | "Draw" the spiral Graph.
spiral :: Int -> String
spiral n = concatMap enfold $ zip line [1..] where
  line = map f [(i,j) | i <- [0..n-1], j <- [0..n-1]]
  graph = spiralGraph n
  f c = if S.member c graph then '*' else ' '
  enfold (ch, p) = [ch, if mod p n == 0 then '\n' else ' ']

main :: IO ()
main = do
  putStr "Size? "
  n <- read <$> getLine
  putStrLn ""
  putStrLn $ spiral n
