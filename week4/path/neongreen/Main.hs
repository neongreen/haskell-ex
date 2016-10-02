{-# LANGUAGE TupleSections #-}

import Data.Array
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Data.Monoid

main = solve (mkArray2d test)

mkArray2d :: [[a]] -> Array (Int, Int) a
mkArray2d xs = listArray ((0,0),(height-1,width-1)) (concat xs)
  where
    width  = length (last xs)
    height = length xs

showArray2d :: Array (Int, Int) Char -> String
showArray2d a = unlines [[a ! (y,x) | x <- [xmin..xmax]] | y <- [ymin..ymax]]
  where
    ((ymin, xmin), (ymax, xmax)) = bounds a

solve :: Array Pos Char -> IO ()
solve field = do
  let Just aPos = fmap fst $ find ((== 'A') . snd) $ assocs field
      Just bPos = fmap fst $ find ((== 'B') . snd) $ assocs field
  let mbPath = floodfill
                 (fmap (== '#') field)    -- obstacle map
                 bPos                     -- cell to find
                 mempty                   -- already visited
                 (M.singleton aPos aPos)  -- frontier; we pretend that we
                                          -- came to A from itself
  putStrLn (showArray2d field)
  case mbPath of
    Nothing   -> putStrLn "path not found"
    Just path -> putStrLn $ showArray2d $
                   field // map (,'+') (init (tail path))

test =
  [ "A.........#.....#...."
  , "...####...#.....#...."
  , "...#........#####...."
  , "...#..#.....#.......#"
  , "...####.##......#...."
  , "#.......#............"
  , "...#........#.....#.."
  , "......#.............."
  , "...#.......#B..#....."
  , ".....#..####........."
  , "#..........###......." ]

type Pos = (Int, Int)

{- |
Here's how the floodfill algorithm works. We have this matrix:

    A#..
    ....
    ....

Initially only the A cell is at the frontier. We mark it with ?.

    ?#..
    ....
    ....

Now we mark the frontier cells as visited and mark all unvisited cells around them as “frontier”:

    0#..
    ?...
    ....

And so it goes, advancing the frontier, until all cells are visited (or rather, until there are no cells at the frontier):

    0       1       2       3       4       5       end

    ?#..    0#..    0#..    0#..    0#?.    0#4?    0#45
    ....    ?...    1?..    12?.    123?    1234    1234
    ....    ....    ?...    2?..    23?.    234?    2345

After it's done, we need to trace our path back to the original cell, so for each visited cell we also store the cell from where we came from.
-}
floodfill
  :: Array Pos Bool    -- ^ Obstacle map; 'True' = obstacle
  -> Pos               -- ^ Cell we want to find
  -> Map Pos Pos       -- ^ Already visited cells, together with “from what
                       --   cell did we come to this cell?”
  -> Map Pos Pos       -- ^ Cells at the “frontier”
  -> Maybe [Pos]       -- ^ Resulting path (reversed)
floodfill field goal visited frontier
  -- If the cell is at the frontier, we can reconstruct the path to it.
  | goal `M.member` frontier = Just (findPath goal visited')
  -- If the frontier is empty and we haven't found the cell before that, the
  -- cell is unreachable.
  | null frontier = Nothing
  -- If the frontier isn't empty but also doesn't contain the cell, we
  -- advance the frontier and keep going.
  | otherwise = floodfill field goal visited' frontier'
  where
    visited' = visited <> frontier
    neighbors (y,x) = [(y-1,x),(y,x-1),(y+1,x),(y,x+1)]
    frontier' = M.fromList [(neighbor, cell)
                               | cell <- M.keys frontier
                               , neighbor <- neighbors cell
                               , inRange (bounds field) neighbor
                               , field ! neighbor == False
                               , neighbor `M.notMember` visited' ]

findPath :: Pos -> Map Pos Pos -> [Pos]
findPath cell m
  | cell == prev = [cell]
  | otherwise    = cell : findPath prev m
  where
    prev = m M.! cell
