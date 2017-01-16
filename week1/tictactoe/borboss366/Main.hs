module Main where

import Data.List (groupBy, maximumBy)
import Data.Function(on)
import Data.Matrix (matrix, Matrix, ncols, nrows, getElem, setElem)
import Data.Char(toLower, isAsciiLower, ord, isDigit)
import System.Console.ANSI (setSGR, ConsoleLayer(..), SGR(..), ColorIntensity(..), Color(..))
import Text.Read(readMaybe)

newtype TTTPoint = TTTPoint {
  getPoint :: (Int, Int)
} deriving (Eq, Show)

newtype TTTDimension = TTTDimension {
  getDimension :: (Int, Int)
}  deriving (Eq, Show)

newtype TTTLine = TTTLine {
  getRow :: [TTTPoint]
} deriving (Eq, Show)

newtype TTTCellLine = TTTCellLine {
  getCellRow :: [(TTTCellType, TTTPoint)]
} deriving (Eq, Show)

data TTTPlayer = Krestig | Nolek deriving (Show, Eq)
data TTTCellType = Occupied TTTPlayer | EmptyCell deriving (Show, Eq)
data TTTMoveState = Move TTTPlayer | Win TTTPlayer | TTTie deriving (Eq, Show)

data TTTPosition = TTTPosition {
  getField :: Matrix TTTCellType,
  getMoves :: [TTTPoint],
  get2Win :: Int
} deriving (Eq, Show)

startGame :: Int -> Int -> Int -> TTTPosition
startGame w h r = TTTPosition {
  getField = matrix h w (const EmptyCell),
  getMoves = [],
  get2Win = r
}

calcDimension :: TTTPosition -> TTTDimension
calcDimension pos = TTTDimension (ncols field, nrows field)
                    where field = getField pos

calcSq :: TTTDimension -> Int
calcSq (TTTDimension (w, h)) = w * h

calcTotalPos :: TTTPosition -> Int
calcTotalPos = calcSq . calcDimension

calcPlayLength :: TTTPosition -> Int
calcPlayLength = length . getMoves

isOccupied :: TTTCellType -> Bool
isOccupied (Occupied _) = True
isOccupied _ = False

isFinished :: TTTMoveState -> Bool
isFinished (Win _) = True
isFinished TTTie = True
isFinished _ = False

getRows :: TTTDimension -> [TTTLine]
getRows (TTTDimension (w, h)) = [mkRow (0, y) (w - 1, y) | y <- [0..h - 1]]
  where
    mkRow p1 p2 = calcLineElems (TTTPoint p1) (TTTPoint p2)

getLines :: TTTDimension -> [TTTLine]
getLines (TTTDimension (w, h)) = rows ++ cols ++ rup ++ rd
  where
    mkRow p1 p2 = calcLineElems (TTTPoint p1) (TTTPoint p2)
    rows =  [mkRow (0, y) (w - 1, y) | y <- [0..h - 1]]
    cols =  [mkRow (x, 0) (x, h - 1) | x <- [0..w - 1]]
    getUpS ind = (max 0 (ind - (h - 1)), min ind (h - 1))
    getUpE ind = (min (w - 1) ind, max (ind - (w - 1)) 0)
    getDS ind = (max 0 (ind - (h - 1)), max 0 ((h - 1) - ind))
    getDE ind = (min ind (w - 1), min (h - 1) ((w - 1) + (h - 1) - ind))
    rup = [mkRow (getUpS ind) (getUpE ind) | ind <- [0 .. w + h - 2]]
    rd =  [mkRow (getDS ind) (getDE ind) | ind <- [0 .. w + h - 2]]

calcLineElems :: TTTPoint -> TTTPoint -> TTTLine
calcLineElems p1 p2 = TTTLine [TTTPoint (posX ind, posY ind) | ind <- [0 .. mInd]]
  where
    (x1, y1) = getPoint p1
    (x2, y2) = getPoint p2
    dx = x2 - x1
    dy = y2 - y1
    (d1, d2) = (signum dx, signum dy)
    posX ind = x1 + d1 * ind
    posY ind = y1 + d2 * ind
    mInd = max (abs dx) (abs dy)

toCellLine :: TTTPosition -> TTTLine -> TTTCellLine
toCellLine pos line = TTTCellLine $ map (\p -> (getCell pos p, p)) $ getRow line

getMaxSubCellLine :: TTTCellType -> TTTCellLine -> TTTCellLine
getMaxSubCellLine ct cl
  | null fltLine = TTTCellLine []
  | otherwise = TTTCellLine $ maximumBy (compare `on` length) fltLine
  where
    grLine = groupBy (\(c1,_) (c2,_) -> c1 == c2) (getCellRow cl)
    fltLine = filter ((== ct).fst.head) grLine

getMaxSubLine :: TTTPosition -> TTTCellType -> TTTLine -> TTTCellLine
getMaxSubLine pos ct line = getMaxSubCellLine ct $ toCellLine pos line

calcWinningLine :: TTTPosition -> Maybe (TTTLine, TTTPlayer)
calcWinningLine pos
  | length krLine >= row2Win = Just (TTTLine krLine, Krestig)
  | length nlLine >= row2Win = Just (TTTLine nlLine, Nolek)
  | otherwise = Nothing
  where
    linez = (getLines.calcDimension) pos -- got all the lines
    maxL = maximumBy (compare `on` (length.getCellRow))
    maxLine ct = map snd.getCellRow $ maxL $ map (getMaxSubLine pos ct) linez
    krLine = maxLine (Occupied Krestig)
    nlLine = maxLine (Occupied Nolek)
    row2Win = get2Win pos

calcMoveState :: TTTPosition -> TTTMoveState
calcMoveState pos =
  case calcWinningLine pos of
    Just line -> Win (snd line)
    Nothing -> nxtMove
  where
    totalC = calcTotalPos pos
    countC = length . getPositions pos
    [kcount, ncount] = map countC [Occupied Krestig, Occupied Nolek]
    nxtMove | totalC <= kcount + ncount = TTTie
            | kcount > ncount = Move Nolek
            | otherwise = Move Krestig

isValidPoint :: TTTPosition -> TTTPoint -> Bool
isValidPoint pos point =
  x >= 0 && y >= 0 && x < w && y < h
  where TTTDimension (w, h) = calcDimension pos
        TTTPoint (x, y) = point

makeMove :: TTTPosition -> TTTPoint -> TTTPosition
makeMove pos point@(TTTPoint (x, y)) =
  if isValidPoint pos point then
    case calcMoveState pos of
      Move player ->
        TTTPosition field' moves' (get2Win pos)
        where
          field' = setElem (Occupied player) (y + 1, x + 1) (getField pos)
          moves' = getMoves pos ++ [point]
      _ -> pos
  else pos

getCell :: TTTPosition -> TTTPoint -> TTTCellType
getCell pos (TTTPoint (x, y)) = getElem (y + 1) (x + 1) (getField pos)

getPositions :: TTTPosition -> TTTCellType -> [TTTPoint]
getPositions pos ct =
  [TTTPoint (i, j) | i <- [0 .. w - 1], j <- [0 .. h - 1], valid i j]
  where TTTDimension (w, h) = calcDimension pos
        valid i j = getCell pos (TTTPoint (i, j)) == ct

getPossibleMoves :: TTTPosition -> [TTTPoint]
getPossibleMoves pos =
  case calcMoveState pos of
    Move _ -> getPositions pos EmptyCell
    _ -> []

{-- Visualisation stuff--}
outputStringColor :: String -> Color -> IO ()
outputStringColor str color = do
  setSGR [SetColor Foreground Vivid color]
  putStr str
  setSGR [Reset]

outputPlayer :: TTTPlayer -> IO ()
outputPlayer Krestig = outputStringColor "X" Red
outputPlayer Nolek = outputStringColor "O" Green

outputCellType :: TTTCellType -> IO ()
outputCellType (Occupied player) = outputPlayer player
outputCellType EmptyCell = putStr " "

outputWinner :: TTTPlayer -> IO ()
outputWinner player = putStr "Thanks for playing! Winner is " >> outputPlayer player >> putStrLn ""

outputDraw :: IO ()
outputDraw = putStr "Thanks for playing!" >> outputStringColor "Deuce" Yellow >> putStrLn ""


outputConfig :: TTTPosition -> IO ()
outputConfig pos = do
  putStr $ "[" ++ show w ++ " x " ++ show h ++ "]. " ++
    "Collect " ++ show in2Win ++ " in row to win. "
  outputPlayer Krestig
  putStrLn " starts"
  where
    (w, h) = (getDimension.calcDimension) pos
    in2Win = get2Win pos

outputUpperIndex :: Int -> IO ()
outputUpperIndex columnCount = putStrLn indexes
  where indexes = "  " ++ concatMap (: " ") (take columnCount ['A'..])

outputRow :: String -> String -> String -> Int -> IO()
outputRow cs cm ce cc
  | cc >= 1 = putStrLn $ " " ++ cs ++ concat (replicate (cc - 1) cm) ++ ce
  | otherwise = putStrLn ""

outputUpperRow :: Int -> IO ()
outputUpperRow = outputRow "┏" "━┯" "━┓"

outputLowerRow :: Int -> IO ()
outputLowerRow = outputRow "┗" "━┷" "━┛"

outputMiddleRow :: Int -> IO ()
outputMiddleRow = outputRow "┠" "─┼" "─┨"

getRowIndex :: TTTPoint -> Int
getRowIndex = snd.getPoint

outputFieldRow :: TTTCellLine -> IO()
outputFieldRow (TTTCellLine row)
  | null row = putStrLn ""
  | otherwise = do
      putStr $ show $ getRowIndex ((snd.head) row) + 1
      putStr "┃"
      mapM_ (\(ttc,_) -> outputCellType ttc >> putStr "|") (init row)
      outputCellType ((fst.last) row)
      putStrLn "┃"

outputMinimax :: Double -> IO()
outputMinimax = print

outputPos :: TTTPosition -> IO ()
outputPos pos = do
  outputUpperIndex w
  outputUpperRow w
  mapM_ (\fr -> outputFieldRow fr >> outputMiddleRow w) (init rows)
  outputFieldRow (last rows)
  outputLowerRow w

  where
    (w, _) = (getDimension.calcDimension) pos
    rows = map (toCellLine pos) $ getRows (calcDimension pos)
{-- End of visualization stuff --}

{-- Parsing stuff --}
parseX :: String -> Maybe Int
parseX str  | length str == 1 && isAsciiLower c = Just $ ord c - ord 'a'
            | otherwise = Nothing
            where c = toLower $ head str


parsePosition :: String -> Maybe TTTPoint
parsePosition line
  | length values == 2 =
    do
      let [xStr, yStr] = values
      xm <- parseX xStr
      ym <- readMaybe yStr
      return (TTTPoint (xm, ym - 1))
  | otherwise = Nothing
  where
    g c1 c2 = isDigit c1 == isDigit c2
    values = groupBy g line

-- Sorry, Artoym, I didn't understand how you made the following code work
-- the binding [xStr, yStr] <- groupBy g line
-- seems strange to me
{--
parsePosition line = do
  let g c1 c2 = isDigit c1 == isDigit c2
  [xStr, yStr] <- groupBy g line
  xm <- parseX xStr
  ym <- readMaybe yStr
  return (TTTPosition x (ym-1))
--}

parseConfig :: String -> Maybe (Int, Int, Int)
parseConfig line |  length values == 3 = do
                      let [ws, hs, rs] = values
                      w <- readMaybe ws
                      h <- readMaybe hs
                      r <- readMaybe rs
                      return (w,h,r)
                 |  otherwise = Nothing
                 where values = words line

readUntilGood :: String -> (String -> Maybe a) -> IO a
readUntilGood title parseFun = do
  putStrLn title
  line <- getLine
  case parseFun line of
    Just object -> return object
    Nothing -> readUntilGood title parseFun
{-- End of parsing stuff--}

{-- the solving functions --}

solveFirstEmpty :: TTTPosition -> Maybe TTTPosition
solveFirstEmpty pos =
  case getPossibleMoves pos of
    (m:_) -> Just $ makeMove pos m
    [] -> Nothing

solveMinimax :: TTTPosition -> Maybe TTTPosition
solveMinimax pos =
  case calcMoveState pos of
    Move _ -> Just $ makeMove pos (fst bestMove)
    _ -> Nothing
  where
    mvz = map (\m -> (m, negate $ minimax $ makeMove pos m)) (getPossibleMoves pos)
    bestMove = maximumBy (compare `on` snd) mvz

minimax :: TTTPosition -> Double
minimax pos =
  case calcMoveState pos of
    Move _ -> snd ourMove
    Win _ -> -1 -- opponent already won
    TTTie -> 0
  where
    -- get the positive values
    -- opp always chooses the move with maximum
    makeMoveCalc move = (move, (negate.minimax) (makeMove pos move))
    moves = map makeMoveCalc (getPossibleMoves pos)
    ourMove = maximumBy (compare `on` snd) moves

playUntilEnd :: TTTPosition -> (TTTPosition -> Maybe TTTPosition) -> TTTPlayer -> IO ()
playUntilEnd pos compFunc compStep =
  case calcMoveState pos of
    Move player -> if player == compStep then
      case compFunc pos of
        Just newPos -> playUntilEnd newPos compFunc compStep
        Nothing -> playUntilEnd pos compFunc compStep
      else do
        outputPos pos
        point <- readUntilGood "Input pos" parsePosition
        playUntilEnd (makeMove pos point) compFunc compStep

    Win player -> do
      outputPos pos
      outputWinner player

    TTTie -> do
      outputPos pos
      outputDraw

main :: IO ()
main = do
  putStrLn "Let's play TicTacToe"
  (w, h, r) <- readUntilGood "Input width, height and winning row length: " parseConfig
  let startPos = startGame w h r
  putStr "Starting game with config " >> outputConfig startPos
  playUntilEnd startPos solveMinimax Nolek
