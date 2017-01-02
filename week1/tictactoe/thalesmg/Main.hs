{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Array as A
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Array (Array)
import           Data.Maybe (maybe, isJust, isNothing, mapMaybe, catMaybes)
import           Data.Foldable (toList)
import           Control.Monad (sequence_, replicateM)
import           System.Console.ANSI
import           System.Random
import           Control.Monad.State 
import qualified Data.Attoparsec.Text as AT
import           Data.Text (Text, pack)

data Move = Computer | Human deriving (Eq, Ord)
instance Show Move where
  show Computer = "O"
  show Human    = "X"
type Pos = (Int, Int)
-- |Nothing denotes a blank position.
type Board = Array Pos (Maybe Move)
data Status = Ongoing | Draw | Won Move deriving (Show, Eq, Ord)
data WrongMove = PositionTaken Pos
               | GameOver Status Board
               deriving (Show, Eq, Ord)
-- |Number of simulations to determine the computer moves.
numSims = 803

blankBoard :: Board
blankBoard = A.listArray ((1, 1), (3, 3)) (repeat Nothing)

-- |Inserts a move in the board. It can signal whether the move is invalid or the game is over.
makeMove :: Move -> Pos -> Board -> Either WrongMove Board
makeMove m p b
  | taken = Left (PositionTaken p)
  | gameOver = Left (GameOver status newBoard)
  | otherwise = Right newBoard
  where
    move = b A.! p
    taken = isJust move
    newBoard = b A.// [(p, Just m)]
    status = checkBoard newBoard m p
    gameOver = status `elem` [Draw, Won Human, Won Computer]

-- |The last move made is used to check whether the game is over.
checkBoard :: Board -> Move -> Pos -> Status
checkBoard b m (x,y) =
  let
    ((minx, _), (maxx, _)) = A.bounds b
    size = maxx - minx + 1
    -- Number of horizontal points
    countH = length [m | ((_, w), Just move) <- A.assocs b, move == m, w == y]
    -- Number of vertical points
    countV = length [m | ((h, _), Just move) <- A.assocs b, move == m, h == x]
    -- We check both diagonals if x == y
    countD = length [m | ((h, w), Just move) <- A.assocs b, x == y, h == w, move == m]
    -- The anti-diagonal has x + y == d + 1, where d is the number of rows/columns of the square matrix.
    countAD = length [m | ((h, w), Just move) <- A.assocs b, x + y == size + 1, h + w == size + 1, move == m]
    allCounts = [countH, countV, countAD, countD]
    -- Is the board filled?
    isFilled = (== size * size) . length . filter isJust $ A.elems b
  in
    if any (== size) allCounts
      then Won m
      else if isFilled
        then Draw
        else Ongoing

-- |Converts the abstract position to the string representation indices.
posToCoord :: Pos -> (Int, Int)
posToCoord (x, y) =
  let
    step = 2
    (x0, y0) = (1, 1)
    (h0, w0) = (3, 6)
    x' = (x - x0) * step + h0
    y' = (y - y0) * step + w0
  in
    (x', y')

blankDiagram :: [String]
blankDiagram =
  ["     A B C \n",
   "    ┏━┯━┯━┓\n",
   "   1┃ │ │ ┃\n",
   "    ┠─┼─┼─┨\n",
   "   2┃ │ │ ┃\n",
   "    ┠─┼─┼─┨\n",
   "   3┃ │ │ ┃\n",
   "    ┗━┷━┷━┛\n"]

printBoard :: Board -> IO ()
printBoard b =
  let
    (h, w) = (length blankDiagram, length (head blankDiagram))
    aDiagram :: Array (Int, Int) Char
    aDiagram = A.listArray ((1, 1), (h, w)) (concat blankDiagram)
    justMoves = filter (isJust . snd) (A.assocs b)
    -- The list of positions and X/O's
    moves = zip (fst <$> justMoves) (catMaybes $ snd <$> justMoves)
    -- We create a list of IO prints to replace the right positions later.
    lstPrnts :: [[IO ()]]
    lstPrnts = map (map putChar) blankDiagram
    -- Converting to an Array for easier indexing.
    aPrnts = A.listArray ((1, 1), (h, w)) (concat lstPrnts)
    -- Substitute the Xs and Os with colored versions
    colorize :: Move -> IO ()
    colorize m = do
      let color = if m == Human then Green else Red
      setSGR [SetColor Foreground Vivid color]
      putStr . show $ m
      setSGR [Reset]
    substitutions = map (\(p, m) -> (posToCoord p, colorize m)) moves
    aPrnts' = aPrnts A.// substitutions
  in
    sequence_ (toList aPrnts')

-- |https://wiki.haskell.org/Random_shuffle
fisherYatesStep :: RandomGen g => (M.Map Int a, g) -> (Int, a) -> (M.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m M.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen
 
fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (M.elems x, y)
    numerate = zip [1..]
    initial x gen = (M.singleton 0 x, gen)

-- |Fills the board until the game is over, and returns the result and the first move in the sequence.
simulate :: (RandomGen g) => Board -> g -> ((Status, Pos), g)
simulate b g =
  let
    -- ^Available positions
    available = fst <$> filter (isNothing . snd) (A.assocs b)
    (rnd, g') = fisherYates g available
    monteCarlo = zip rnd (cycle [Computer, Human])
    go :: Board -> [(Pos, Move)] -> (Status, Board)
    go board [] = (Draw, board)
    go board ((p, m):ps) = case makeMove m p board of
      Left (GameOver status board')   -> (status, board')
      Left (PositionTaken pos) -> undefined
      Right board'             -> go board' ps
    (status, finalBoard)  = go b monteCarlo
    firstMove = head rnd
  in
    ((status, firstMove), g')

-- |Counts the frequencies of each outcome for each next computer move.
frequencies :: [(Status, Pos)] -> Map Status (Map Pos Double)
frequencies lst =
  let
    go :: Pos -> Map Pos Double -> Map Pos Double
    go p acc = M.insertWith (+) p 1 acc
    wins = snd <$> filter ((== Won Computer) . fst) lst
    draws = snd <$> filter ((== Draw) . fst) lst
    loses = snd <$> filter ((== Won Human) . fst) lst
    possibilities = [(Won Computer, wins), (Draw, draws), (Won Human, loses)]
    counts = foldr (\(status, xs) acc -> M.insert status (foldr go M.empty xs) acc) M.empty possibilities
    total = fromIntegral . length $ lst
  in
    M.map (M.map (/ total)) counts

-- |Returns the best move (highest probability) that either wins or draws the game.
bestMove :: Map Status (Map Pos Double) -> Pos
bestMove m =
  let
    go :: [Pos] -> Maybe Double -> [(Pos, Double)] -> [Pos]
    go ks _ [] = ks
    go ks Nothing ((k, v):kvs) = go (k:ks) (Just v) kvs
    go ks (Just best) ((k, v):kvs)
      | best > v = go ks (Just best) kvs
      | best < v = go [k] (Just v) kvs
      | otherwise = go (k:ks) (Just v) kvs
    go0 status = go [] Nothing (M.assocs (m M.! status))
    winningMoves = go0 (Won Computer)
    drawMoves = go0 (Draw)
    losingMoves = go0 (Won Human)
  in
    head $ winningMoves `mappend` drawMoves `mappend` losingMoves

parseMove :: Text -> AT.Result Pos
parseMove = AT.parse parsePlay
  where
    colParse = AT.satisfy (\c -> c == 'A' || c == 'B' || c == 'C')
    rowParse = AT.satisfy (\c -> c == '1' || c == '2' || c == '3')
    colMap = M.fromList [('A', 1), ('B', 2), ('C', 3)]
    parsePlay :: AT.Parser Pos
    parsePlay = do
      c <- colParse
      r <- rowParse
      return (read [r], colMap M.! c)

mainLoop :: (RandomGen g) => Board -> g -> IO ()
mainLoop b g = do
  printBoard b
  play b g
  where
    getMove :: IO Pos
    getMove = do
      move <- pack <$> getLine
      p <- case parseMove move of
        AT.Done _ p' -> return p'
        _            -> do
                          putStr "Invalid move. Try again.\n> "
                          getMove
      return p
    computerPlay :: (RandomGen g) => Board -> g -> (Board, g)
    computerPlay board g =
      let
        s = state $ simulate board
        (simulations, g') = runState (replicateM numSims s) g
        bMove = bestMove . frequencies $ simulations
        Right board' = makeMove Computer bMove board
      in
        (board', g')
    play :: (RandomGen g) => Board -> g -> IO ()
    play b g = do
      putStr "Your move:\n> "
      p <- getMove
      case makeMove Human p b of
        Left (PositionTaken _) -> do
                                    putStrLn "That position is taken!"
                                    play b g
        Left (GameOver s b'') ->
          do
            printBoard b''
            case s of
              Draw -> putStrLn "Draw!"
              Won Human -> putStrLn "You won!"
              Won Computer -> putStrLn "Computer won!"
        Right b' -> do
                      let (b'', g') = computerPlay b' g
                      printBoard b''
                      play b'' g'

main :: IO ()
main = do
  g <- getStdGen
  mainLoop blankBoard g
