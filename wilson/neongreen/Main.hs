{-# LANGUAGE MultiWayIf #-}

import Data.Foldable
import Data.Array
import Data.Random                  -- from random-fu
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Lens.Micro                   -- from microlens


type Size = (Int, Int)  -- width, height
type Pos  = (Int, Int)  -- X, Y

data Arrow = U | D | L | R | Start
  deriving (Eq, Show)

randomWalk
  :: Size              -- ^ Size of the field
  -> Pos               -- ^ Starting cell
  -> (Pos -> Bool)     -- ^ When to stop
  -> RVar [(Pos, Arrow)]
randomWalk size pos stop = go pos
  where
    bounds = ((0, 0), (fst size - 1, snd size - 1))

    neighbors :: Pos -> [(Pos, Arrow)]
    neighbors (x,y) = filter (inRange bounds . fst) [
      ((x,y-1), U), ((x,y+1), D),
      ((x-1,y), L), ((x+1,y), R) ]

    go :: Pos -> RVar [(Pos, Arrow)]
    go p = do
      (p', d) <- randomElement (neighbors p)
      if stop p' then return [(p, d)] else ((p, d):) <$> go p'

-- | Simplify the walk by cutting out the cycles. If a cell is encountered
-- several times in the walk, then the whole segment from the first encounter
-- to the last encounter (but not including it) will be removed.
--
-- 12345678    1|23456|78    178
-- ABCBDDBE -> A|BCBDD|BE -> ABE
simplifyWalk :: [(Pos, Arrow)] -> [(Pos, Arrow)]
simplifyWalk ps = go 0
  where
    l = length ps
    -- For each position, find the last index at which it's encountered
    lastEncounter :: Map Pos Int
    lastEncounter = M.fromList (zip (map fst ps) [0..])
    -- Convert the list of positions to an array (so that it'll be faster
    -- to access).
    arr :: Array Int (Pos, Arrow)
    arr = listArray (0, l-1) ps
    -- Go through the array, yield items as they come (and jump sometimes).
    go :: Int -> [(Pos, Arrow)]
    go i | i >= l    = []
         | otherwise =
             let (p, _) = arr ! i
                 j = lastEncounter M.! p  -- where to jump (might be i==j)
             in  (arr ! j) : go (j+1)

wilson :: Size -> IO (Array (Int, Int) Arrow)
wilson size@(width, height) = sample $ do
  startCell <- (,) <$> uniform 0 (width-1) <*> uniform 0 (height-1)
  fill (startArray startCell)
  where
    startArray :: (Int, Int) -> Array (Int, Int) (Maybe Arrow)
    startArray startCell =
      listArray ((0,0),(width-1,height-1)) (repeat Nothing)
      // [(startCell, Just Start)]

    fill :: Array (Int, Int) (Maybe Arrow)
         -> RVar (Array (Int, Int) Arrow)
    fill arr = do
      let empties = map fst $ filter (isNothing . snd) (assocs arr)
      case empties of
        []    -> return (fmap fromJust arr)
        (x:_) -> do
          walk <- randomWalk size x (\p -> isJust (arr ! p))
          fill (arr // map (over _2 Just) (simplifyWalk walk))

printArrows :: Array (Int, Int) Arrow -> IO ()
printArrows arr = do
  let (lastX, lastY) = snd (bounds arr)
  for_ [0 .. lastY] $ \y -> do
    for_ [0 .. lastX] $ \x ->
      case arr ! (x, y) of
        U     -> putChar '↑'
        D     -> putChar '↓'
        L     -> putChar '←'
        R     -> putChar '→'
        Start -> putChar '•'
    putStrLn ""

printMaze :: Array (Int, Int) Arrow -> IO ()
printMaze arr = do
  let (lastX, lastY) = snd (bounds arr)
  putStrLn (" " ++ replicate (2*lastX+1) '_')
  for_ [0 .. lastY] $ \y -> do
    putChar '|'
    for_ [0 .. lastX] $ \x -> do
      if | y == lastY                             -> putChar '_'
         | arr ! (x,y) /= D && arr ! (x,y+1) /= U -> putChar '_'
         | otherwise                              -> putChar ' '
      if | x == lastX                             -> putChar '|'
         | arr ! (x,y) /= R && arr ! (x+1,y) /= L -> putChar '|'
         | otherwise                              -> putChar ' '
    putStrLn ""
