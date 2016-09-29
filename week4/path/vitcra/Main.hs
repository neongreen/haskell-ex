{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Array
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Monad
import Data.List.Split

type Index = (Int, Int)

data Infinite a = Infinity | Finite a deriving (Show)

data ProblemState = ProblemState
  { dist :: Array Index (Infinite Int)
  , edges :: Array Index Char
  , unvisited :: Set (Infinite Int, Index)
  , prev :: Map Index Index } deriving (Show)

instance Eq a => Eq (Infinite a) where
  Infinity == Infinity = True
  (Finite a) == (Finite b) = a == b
  _ == _ = False

instance Ord a => Ord (Infinite a) where
  compare Infinity Infinity = EQ
  compare Infinity _ = GT
  compare _ Infinity = LT
  compare (Finite a) (Finite b) = compare a b

blockerCh = '#'
startCh = 'A'
endCh = 'B'

add :: Num a => Infinite a -> Infinite a -> Infinite a
add Infinity _ = Infinity
add _ Infinity = Infinity
add (Finite a) (Finite b) = Finite $ a+b

initialize :: [String] -> (Int, Index, Index, ProblemState)
initialize [] = undefined
initialize ss@(s:_) = (n, start, end, ProblemState {..}) where
  m = length ss
  n = length s
  input = concat ss
  bnds = ((0, 0), (m-1,n-1))
  edges = listArray bnds input
  start = divMod (fromMaybe 0 $ elemIndex startCh input) n
  end  = divMod (fromMaybe 0 $ elemIndex endCh input) n
  unvisited = S.singleton (Finite 0, start)
  dist = listArray bnds (repeat Infinity) // [(start, Finite 0)]
  prev = M.empty


inBounds :: Index -> (Index, Index) -> Bool
inBounds (i,j) ((ai, aj), (bi, bj)) = i>=ai && i<=bi && j>=aj && j<=bj

neighbours :: Array Index Char -> Index -> [Index]
neighbours eds (r,c) = do
  (i, j) <- [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
  let bnds = bounds eds
  guard ( (i,j) `inBounds` bnds && eds ! (i,j) /= blockerCh)
  return (i, j)


-- | Nothing = we can go on searching
-- | Just False = there's no path
-- | Just True = the path was found
go :: ProblemState -> (Maybe Bool, ProblemState)
go pst@ProblemState{..}
  | null unvisited = (Just False, pst)
  | otherwise = (found, newstate)
  where
    ((cd, ci), moretovisit) = S.deleteFindMin unvisited
    nbs = neighbours edges ci
    d = add (Finite 1) cd
    upds = [i | i <- nbs, dist ! i > d]

    found
      | cd == Infinity = Just False
      | endCh == edges ! ci = Just True
      | otherwise = Nothing

    newstate = ProblemState
      { dist = dist // zip upds (repeat d)
      , edges = edges // [(ci, blockerCh)]
      , unvisited = S.union moretovisit (S.fromList $ zip (repeat d) upds)
      , prev = M.union prev (M.fromList $ zip upds (repeat ci)) }

tracePath :: (Index, Index) -> Map Index Index -> [Index]
tracePath (start, end) prevs = ps where
  ps = reverse $ helper (prevs M.! end) []
  helper i acc
    | i == start = acc
    | otherwise = i : helper (prevs M.! i) acc

findPath :: (Maybe Bool, ProblemState) -> Map Index Index
findPath (Nothing, st) = findPath $ go st
findPath (Just False, st) = M.empty
findPath (Just True, st) = prev st

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

main :: IO ()
main = do
  contents <- getContents
  putStrLn contents
  putStrLn "\n"
  let (ncols, start, end, initialState) = initialize $ lines contents
      path = tracePath (start, end) $ findPath (Nothing, initialState)
      board = edges initialState
  if null path
    then putStrLn "There's no path."
    else putStrLn . unlines . chunksOf ncols . elems $ board // zip path (repeat '+')
