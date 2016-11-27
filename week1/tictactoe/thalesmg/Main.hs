import qualified Data.Array as A
import           Data.Array (Array)
import           Data.Maybe (maybe, isJust, mapMaybe, catMaybes)
import           Data.Foldable (toList)
import           Control.Monad (sequence_)
import           System.Console.ANSI

data Move = O | X deriving (Show, Eq, Ord)
type Pos = (Int, Int)
type Board = Array Pos (Maybe Move)
data Status = Ongoing | Draw | Won Move deriving (Show, Eq, Ord)

blankBoard :: Board
blankBoard = A.listArray ((1, 1), (3, 3)) (repeat Nothing)

makeMove :: Move -> Pos -> Board -> Maybe Board
makeMove m p b =
  let
    move = b A.! p
    taken = isJust move
  in
    if taken
      then Nothing
      else Just $ b A.// [(p, Just m)]

-- |The last move made is used to check whether the game is over.
checkBoard :: Board -> Move -> Pos -> Status
checkBoard b m (x,y) =
  let
    ((minx, _), (maxx, _)) = A.bounds b
    size = maxx - minx + 1
    countH = length [m | ((_, w), Just move) <- A.assocs b, move == m, w == y]
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
    (h, w) = (length blankDiagram, length (blankDiagram !! 0))
    aDiagram :: Array (Int, Int) Char
    aDiagram = A.listArray ((1, 1), (h, w)) (concat blankDiagram)
--    game = A.assocs b
    justMoves = filter (isJust . snd) (A.assocs b)
    moves = zip (fst <$> justMoves) (catMaybes $ snd <$> justMoves)
    lstPrnts :: [[IO ()]]
    lstPrnts = map (map putChar) blankDiagram
    aPrnts = A.listArray ((1, 1), (h, w)) (concat lstPrnts)
    -- Substitute the Xs and Os with colored versions
    colorize :: Move -> IO ()
    colorize m = do
      let color = if m == X then Green else Red
      setSGR [SetColor Foreground Vivid color]
      putStr . show $ m
      setSGR [Reset]
    substitutions = map (\(p, m) -> (posToCoord p, colorize m)) moves
    aPrnts' = aPrnts A.// substitutions
  in
    sequence_ (toList aPrnts')


main :: IO ()
main = return ()
