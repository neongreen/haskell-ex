module Main where

import           Control.Monad
import           Data.Char           (toUpper)
import           Data.Function
import           Data.List
import           Data.Matrix         (Matrix)
import qualified Data.Matrix         as M
import           Data.Maybe
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           System.Console.ANSI
import System.IO

data Player = Computer | Human deriving (Enum, Eq)
data Outcome = Lose | Draw | Win deriving (Eq, Ord, Show)
type Board = Matrix (Maybe Player)

goal :: Player -> Outcome
goal Computer = Win
goal Human = Lose

-- | Returns all 'tictactoe' vectors from a Matrix
tictactoes :: Matrix a -> [Vector a]
tictactoes board = diag1:diag2:(rows ++ cols) where
  m = M.nrows board
  n = M.ncols board
  rows = map (`M.getRow` board) [1..m]
  cols = map (`M.getCol` board) [1..n]
  diag1 = M.getDiag board
  diag2 = V.fromList $ map (board M.!) $ zip [1..m] [n, n-1..1]

evalTriplet :: Vector (Maybe Player) -> Outcome
evalTriplet ts = case mplayer of
  (Just p) -> goal p
  Nothing -> Draw
  where
    first = V.head ts
    mplayer = if V.all (== first) ts then first else Nothing

evalBoard :: Board -> Maybe Outcome
evalBoard board = case os of
  [] -> if fullBoard board then Just Draw else Nothing
  (o:_) -> Just o
  where
    os = dropWhile (== Draw) $ map evalTriplet $ tictactoes board

fullBoard :: Board -> Bool
fullBoard = (Nothing `notElem`) . M.toList

-- | Given a board, return all possible boards after Player move
nextMoves :: Player -> Board -> [Board]
nextMoves player board = do
  i <- [1..M.nrows board]
  j <- [1..M.ncols board]
  guard . isNothing . (board M.!) $ (i, j)
  return $ M.setElem (Just player) (i, j) board

theOther :: Player -> Player
theOther Computer = Human
theOther Human = Computer

minimax :: Board -> Board
minimax board = if fullBoard board then board else snd $ helper Computer board
  where
    helper :: Player -> Board -> (Outcome, Board)
    helper player board = best where
      pick = case player of
        Computer -> maximumBy
        Human -> minimumBy

      ebs = map (\b -> (evalBoard b, b)) $ nextMoves player board
      (greedy, undecided) = partition ((== (Just $ goal player)) . fst) ebs

      rebs = map reeval undecided
      reeval (o, b) = case o of
        Just smth -> (smth, b)
        Nothing -> (fst $ helper (theOther player) b, b)

      best = case greedy of
        ((Just o, b):_) -> (o, b)
        [] -> pick (compare `on` fst) rebs


-- | Boards other than 3 by 3 will look ugly...
printBoard :: Board -> IO ()
printBoard b = do
  putStrLn "\n  A B C \n ┏━┯━┯━┓"
  mapM_ printRow [1..m]
  putStrLn " ┗━┷━┷━┛\n"
  where
    m = M.nrows b
    n = M.ncols b

    printCell (i, j) = do
      case b M.! (i, j) of
        Just Computer -> do
          setSGR [SetColor Foreground Vivid Green]
          putStr "O"
          setSGR [Reset]
        Just Human -> do
          setSGR [SetColor Foreground Dull Red]
          putStr "X"
          setSGR [Reset]
        Nothing -> putStr " "
      when (j /= n) $ putStr "|"

    printRow i = do
      putStr $ show i ++ "┃"
      mapM_ (\j -> printCell (i, j)) [1..n]
      putStrLn "┃"

parseMove :: String -> Maybe (Int, Int)
parseMove [c,r] = ((,) `on` (+1))
  <$> elemIndex r "123"
  <*> elemIndex (toUpper c) "ABC"
parseMove _ = Nothing

loop :: Board -> IO ()
loop board = do
  printBoard board
  case evalBoard board of
    Just Win -> putStrLn "Computer wins."
    Just Lose -> putStrLn "You win."
    Just Draw -> putStrLn "It's a draw."
    Nothing -> do
      putStr "Your move>"
      move <- getLine
      let pos = parseMove move
      case pos of
        Nothing -> do
          putStrLn "\nInvalid move.\n"
          loop board
        Just pos -> case board M.! pos of
          (Just _) -> do
            putStrLn "\nCell taken.\n"
            loop board
          Nothing ->
            loop $ minimax (M.setElem (Just Human) pos board)


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  loop $ M.fromList 3 3 $ repeat Nothing
