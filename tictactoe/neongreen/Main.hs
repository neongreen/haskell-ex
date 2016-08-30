{-# LANGUAGE TemplateHaskell #-}

import System.Console.ANSI    -- from ansi-terminal
import Lens.Micro             -- from microlens
import Data.Function.Memoize  -- from memoize
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord

-- | A type for Xs and Os.
data Cell = X | O
  deriving (Eq, Show)

-- This will be needed later, don't bother about it for now.
deriveMemoizable ''Cell

-- | Invert a cell.
invert :: Cell -> Cell
invert X = O
invert O = X

-- | Position: x-coordinate, y-coordinate.
type Pos = (Int, Int)

-- | A playing field is a list of lists of cells. Not very typesafe,
-- yep. Empty cells are denoted with 'Nothing'.
type Field = [[Maybe Cell]]

emptyField :: Field
emptyField = replicate 3 (replicate 3 Nothing)

getCell :: Pos -> Field -> Maybe Cell
getCell (x,y) f = (f !! y) !! x

setCell :: Pos -> Maybe Cell -> Field -> Field
setCell (x,y) c f = set (ix y . ix x) c f

-- | Return all rows, columns, and diagonals of a field, together with
-- coordinates of each cell.
triples :: Field -> [[Maybe Cell]]
triples f =
  -- Rows and columns
  f ++ transpose f ++
  -- Diagonals
  [zipWith (!!) f [0,1,2], zipWith (!!) f [2,1,0]]

-- Note the ordering. “Lose | Tie | Win” means that Win > Tie and Tie > Lose.
data Outcome = Lose | Tie | Win
  deriving (Eq, Ord, Show)

-- | Print a single cell (with coloring)
cell :: Maybe Cell -> IO ()
cell Nothing = putStr " "
cell (Just X) = do
  setSGR [SetColor Foreground Vivid Green]
  putStr "X"
  setSGR [Reset]
cell (Just O) = do
  setSGR [SetColor Foreground Vivid Red]
  putStr "O"
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

-- | If the field has been won, find the winning player.
winner :: Field -> Maybe Cell
winner f
  | [Just X,Just X,Just X] `elem` triples f = Just X
  | [Just O,Just O,Just O] `elem` triples f = Just O
  | otherwise                               = Nothing

-- | Check whether the given player has won, lost, etc.
judge
  :: Cell     -- ^ Player
  -> Field    -- ^ Field
  -> Maybe Outcome
judge c f = case winner f of
  Nothing -> if all isJust (concat f) then Just Tie else Nothing
  Just w  -> if w == c then Just Win else Just Lose

-- | What's the best outcome for a player if it's their turn now? And what's
-- the move that gives that outcome?
outcome :: Cell -> Field -> (Maybe Pos, Outcome)
outcome =
  -- Here we use 'memoize2' to create a lookup table – if we already computed
  -- the outcome for this position, we won't recompute it.
  memoize2 $ \c f -> case judge c f of
    -- If we won already, well, we won. If someone else won and that's not
    -- us, then we lost. If it's a tie, it's a tie
    Just o -> (Nothing, o)
    -- If nobody won yet, let's try to put 'c' into any of the empty cells
    -- and find the best outcome for us.
    Nothing -> maximumBy (comparing snd) $ do
      x <- [0..2]; y <- [0..2]
      guard (isNothing (getCell (x,y) f))
      let f' = setCell (x,y) (Just c) f
      let theirOutcome = snd (outcome (invert c) f')
          ourOutcome = case theirOutcome of
            Win  -> Lose
            Lose -> Win
            Tie  -> Tie
      return (Just (x, y), ourOutcome)

{- |
>>> parseMove "A2"
(0,2)
-}
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
