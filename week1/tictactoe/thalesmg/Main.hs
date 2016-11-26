import qualified Data.Array as A
import           Data.Array (Array)
import           Data.Maybe (maybe, isJust, mapMaybe, catMaybes)
import qualified System.Console.ANSI as T

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
    (x0, y0) = (2, 5)
    x' = (x - x0) * step
    y' = (y - y0) * step
  in
    (x', y')

blankDiagram :: [String]
blankDiagram =
  ["     A B C",
   "    ┏━┯━┯━┓",
   "   1┃ │ │ ┃",
   "    ┠─┼─┼─┨",
   "   2┃ │ │ ┃",
   "    ┠─┼─┼─┨",
   "   3┃ │ │ ┃",
   "    ┗━┷━┷━┛"]

printBoard :: Board -> IO ()
printBoard b =
  let
    justMoves = filter (isJust . snd) (A.assocs b)
    moves = zip (fst <$> justMoves) (catMaybes $ snd <$> justMoves)
  in
    return ()


main :: IO ()
main = return ()
