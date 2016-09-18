{-# LANGUAGE TemplateHaskell #-}

import System.Console.ANSI    -- from ansi-terminal
import Lens.Micro             -- from microlens
import Data.Function.Memoize  -- from memoize
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord

-- | Crosses vs noughts.
data Player = X | O
  deriving (Eq, Show)

-- This will be needed later, don't bother about it for now.
deriveMemoizable ''Player

-- | Pick the other side.
invert :: Player -> Player
invert X = O
invert O = X

-- | Position: x-coordinate, y-coordinate.
type Pos = (Int, Int)

-- | A playing field is a list of lists of cells. Not very typesafe,
-- yep. Empty cells are denoted with 'Nothing'.
type Field = [[Maybe Player]]

emptyField :: Field
emptyField = replicate 3 (replicate 3 Nothing)

getCell :: Pos -> Field -> Maybe Player
getCell (x,y) f = (f !! y) !! x

setCell :: Pos -> Maybe Player -> Field -> Field
setCell (x,y) c f = set (ix y . ix x) c f

-- | Return all rows, columns, and diagonals of a field.
triples :: Field -> [[Maybe Player]]
triples f =
  -- Rows and columns
  f ++ transpose f ++
  -- Diagonals
  [zipWith (!!) f [0,1,2], zipWith (!!) f [2,1,0]]

-- Note the ordering. “Lose | Tie | Win” means that Win > Tie and Tie > Lose.
data Outcome = Lose | Tie | Win
  deriving (Eq, Ord, Show)

-- | Print a single cell (with coloring)
cell :: Maybe Player -> IO ()
cell c = do
  case c of
    Nothing -> putStr " "
    Just X  -> setSGR [SetColor Foreground Vivid Green] >> putStr "X"
    Just O  -> setSGR [SetColor Foreground Vivid Red]   >> putStr "O"
  setSGR [Reset]

-- | Print the whole field.
printField :: Field -> IO ()
printField f = do
  let printLine i cs = do
        putStr (show i ++ "┃")
        cell (head cs)
        mapM_ (\c -> putStr "│" >> cell c) (tail cs)
        putStrLn "┃"
  putStrLn "  A B C "
  putStrLn " ┏━┯━┯━┓"
  printLine 1 (f!!0)
  putStrLn " ┠─┼─┼─┨"
  printLine 2 (f!!1)
  putStrLn " ┠─┼─┼─┨"
  printLine 3 (f!!2)
  putStrLn " ┗━┷━┷━┛"

-- | If the game has been won, find the winning player.
winner :: Field -> Maybe Player
winner f
  | [Just X,Just X,Just X] `elem` triples f = Just X
  | [Just O,Just O,Just O] `elem` triples f = Just O
  | otherwise                               = Nothing

-- | Check whether the given player has won, lost, etc.
judge :: Player -> Field -> Maybe Outcome
judge p f = case winner f of
  Nothing -> if all isJust (concat f) then Just Tie else Nothing
  Just w  -> if w == p then Just Win else Just Lose

-- | What's the best outcome for a player if it's their turn now? And what's
-- the move that gives that outcome?
outcome :: Player -> Field -> (Maybe Pos, Outcome)
outcome =
  -- Here we use 'memoize2' to create a lookup table – if we already computed
  -- the outcome for this position, we won't recompute it.
  memoize2 $ \p f -> case judge p f of
    -- If we won/lost already or it's a tie, just report that.
    Just o -> (Nothing, o)
    -- If nobody won yet, let's try to put 'p' into any of the empty cells
    -- and find which gives the best outcome for us.
    Nothing -> maximumBy (comparing snd) $ do
      x <- [0..2]; y <- [0..2]
      guard (isNothing (getCell (x,y) f))
      let f' = setCell (x,y) (Just p) f
      let theirOutcome = snd (outcome (invert p) f')
          ourOutcome = case theirOutcome of
            Win  -> Lose
            Lose -> Win
            Tie  -> Tie
      return (Just (x, y), ourOutcome)
    -- By the way, if we gave 'Nothing' when the outcome is 'Lose', the
    -- computer would give up immediately the game is lost.

-- >>> map parseMove ["A2", "B2", "X8"]
-- [Just (0,2), Just (1,2), Nothing]
parseMove :: String -> Maybe Pos
parseMove s = do
  [a,b] <- Just s
  (,) <$> elemIndex a "ABC" <*> elemIndex b "123"

askForMove :: Field -> IO Pos
askForMove f = do
  putStrLn "Your move:"
  putStr "> "
  s <- getLine
  case parseMove s of
    Nothing -> do
      putStrLn "" >> putStrLn ("Couldn't parse " ++ show s) >> putStrLn ""
      askForMove f
    Just (x, y) -> case getCell (x, y) f of
      Just _ -> do
        putStrLn "" >> putStrLn "This cell is already taken!" >> putStrLn ""
        askForMove f
      Nothing -> do
        putStrLn ""
        return (x, y)

gameLoop :: Field -> IO ()
gameLoop f = do
  printField f
  putStrLn ""
  case judge X f of
    Just Tie  -> putStrLn "The game ended in a tie."
    Just Win  -> putStrLn "You won!"
    Just Lose -> putStrLn "Computer won."
    Nothing   -> do
      yourMove <- askForMove f
      let f' = setCell yourMove (Just X) f
      case outcome O f' of
        (Nothing, _) -> gameLoop f'
        (Just computerMove, _) ->
          let f'' = setCell computerMove (Just O) f'
          in  gameLoop f''

main :: IO ()
main = gameLoop emptyField
