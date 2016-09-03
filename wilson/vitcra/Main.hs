{-# LANGUAGE RecordWildCards #-}
module Main where

import           Data.Matrix   (Matrix)
import qualified Data.Matrix   as M
import           Data.Set      (Set)
import qualified Data.Set      as S
import qualified Data.Vector   as V
import           System.Random

data Direction = None | North | East | South | West
  deriving (Eq, Enum, Ord, Show)

type Index = (Int, Int)

data Maze = Maze
  { inside  :: Set Index -- these are already in the maze
  , outside :: Set Index -- not yet in the maze
  , field   :: Matrix Direction} deriving (Show)

-- note that random will never return None
instance Random Direction where
  randomR (a, b) g = (dir, g') where
    (x, g') = randomR (fromEnum a, fromEnum b) g
    dir = toEnum x
  random = randomR (North, West)

-- all indices in a n by n matrix
allIndices :: Int -> Set Index
allIndices n = S.fromList [(row, col) | row <- [1..n], col <- [1..n]]

getRandomIndex :: RandomGen g => g -> Set Index -> (Index, g)
getRandomIndex g xs= (x, g') where
  (p, g') = randomR (0, S.size xs - 1) g
  x = S.elemAt p xs

-- do not go outside the boundary
isDirAllowed :: Int -> Index -> Direction -> Bool
isDirAllowed dim (row, col) dir = case dir of
  North -> row /= 1
  East -> col /= dim
  South -> row /= dim
  West -> col /= 1

getRandomDir ::
  RandomGen g => g -> Int -> Index -> (Direction, g)
getRandomDir g dim x = (dir,  g'') where
  (g', g'') = split g
  (dir:_) = dropWhile (not . isDirAllowed dim x) (randoms g')

-- difference between current index and the neighbour in given direction
dirDelta :: Direction -> (Int, Int)
dirDelta dir = case dir of
  North -> (-1, 0)
  East -> (0, 1)
  South -> (1, 0)
  West -> (0, -1)

step :: Direction -> Index -> Index
step dir (row, col) = (row + rd, col + cd) where
  (rd, cd) = dirDelta dir

-- walk in the current maze from 'start' cell
-- which should be outside the maze
-- return the first maze cell encountered
-- and the maze with directions changed by the walk
walk :: RandomGen g => g -> Maze -> Index -> (Index, Maze, g)
walk g maze@Maze{..} start =
  if S.null inside || S.member start inside
  then (start, maze, g)
  else walk g' newmaze finish
  where
    (dir, g') = getRandomDir g (M.nrows field) start
    finish = step dir start
    newmaze = Maze
      { inside = inside
      , outside = outside
      , field = M.setElem dir start field }

-- now we put into the Maze all cells in the walk
-- we ignore the 'loop' cells
convertWalk :: Maze -> Index -> Index -> Maze
convertWalk initial@Maze{..} start finish =
  if start == finish then initial else convertWalk newmaze next finish
  where
    next = step (field M.! start) start
    newmaze = Maze
      { inside = S.insert start inside
      , outside = S.delete start outside
      , field = field }

generateMaze :: RandomGen g => g -> Maze -> Maze
generateMaze g maze@Maze{..} =
  if S.null outside
  then maze
  else
    generateMaze g'' $ convertWalk nextmaze start finish
  where
    (start, g') = getRandomIndex g outside
    (finish, nextmaze, g'') = walk g' maze start

-- produce an 'empty' maze with only one cell inside
emptyMaze :: RandomGen g => g -> Int -> (Maze, g)
emptyMaze g n = (Maze{..}, g') where
  indices = allIndices n
  (first, g') = getRandomIndex g indices
  inside = S.fromList [first]
  outside = S.delete first indices
  field = M.fromList n n $ repeat None

prettyMaze :: Maze -> String
prettyMaze Maze{..} = concat
  [ " "
  , concat $ replicate n "_ "
  , "\n"
  , concat mapDS ]
  where
    n = M.nrows field
    mapDS = [str (row, col)| row <- [1..n], col <- [1..n]]
    str pos@(row, col)
      | col == 1 = "|" ++ walls
      | col == n = walls ++ "\n"
      | otherwise = walls
      where
        walls = bottom ++ right
        bottom = if field M.! pos == South
          || (row /= n && field M.! (row+1, col) == North)
          then " "
          else "_"
        right = if field M.! pos == East
          || (col /= n && field M.! (row, col+1) == West)
          then " "
          else "|"

main :: IO ()
main = do
  g <- newStdGen
  let
    (initialMaze, g') = emptyMaze g 15
    maze = generateMaze g' initialMaze
  putStrLn $ prettyMaze maze
