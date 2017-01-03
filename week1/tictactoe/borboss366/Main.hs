module Main where

import Data.Maybe(isJust, fromJust)
import Data.List(find, groupBy, maximumBy)
import Data.Function(on)
import Text.Read(readMaybe)
import Data.Char(toLower, isAsciiLower, ord, isDigit)
import System.Console.ANSI (setSGR, ConsoleLayer(..), SGR(..), ColorIntensity(..), Color(..))

{-- Different data types --}
data TTTStepType = Krestig | Nolek deriving (Show, Eq)
data TTTCellType = TTTCellType TTTStepType | EmptyCell | WrongCell deriving (Show, Eq)
data TTTPosition = TTTPosition Int Int deriving (Show, Eq)
data TTTCellPosition = TTTCellPosition TTTCellType TTTPosition deriving (Show, Eq)
newtype TTTRow = TTTRow [TTTCellPosition] deriving (Show)
-- newtype TTTRowView = TTTRowView [(TTTCellPosition, Bool)] -- shows whether the cellposition is finishing
data TTTWinner = TTTWinner TTTStepType  | Deuce | UnknownWinner deriving (Show, Eq)

data TTTStep = TTTStep {
  position :: TTTPosition,
  stepType :: TTTStepType
} deriving (Show, Eq)

data TTTConfig = TTTConfig {
  fieldWidth :: Int,
  fieldHeight :: Int,
  inRow2Win :: Int,
  firstStep :: TTTStepType
} deriving Show

data TTTPlay = TTTPlay {
  config :: TTTConfig,
  steps :: [TTTStep]
} deriving Show

data TTTNextStepInfo = TTTNextStepInfo {
  emptyPositions :: [TTTPosition],
  nextStepType :: TTTStepType
} deriving Show
{-- End of data types--}


isConfigPositionSat :: TTTConfig -> TTTPosition -> Bool
isConfigPositionSat tttC (TTTPosition x y) = isSat x w && isSat y h
  where w = fieldWidth tttC
        h = fieldHeight tttC
        isSat p d = p >= 0 && p < d

calcPosCell :: TTTPlay -> TTTPosition -> TTTCellPosition
calcPosCell tttPlay tp = TTTCellPosition (calcPos tttPlay tp) tp

calcPos :: TTTPlay -> TTTPosition -> TTTCellType
calcPos tttPlay tp = maybe cell (TTTCellType . stepType) (find (\stp -> position stp == tp) $ steps tttPlay)
  where
    cnfSat = isConfigPositionSat (config tttPlay) tp
    cell = if cnfSat then EmptyCell else WrongCell

startPlay :: TTTConfig -> TTTPlay
startPlay cnf = TTTPlay cnf []

movesCount :: TTTPlay -> Int
movesCount tttP = length $ steps tttP

getDiagInd :: TTTConfig -> [Int]
getDiagInd tttC = [0 .. fieldWidth tttC + fieldHeight tttC - 1]

getDiagHelp :: TTTConfig -> TTTPosition -> Int -> [TTTPosition]
getDiagHelp tttC (TTTPosition xs ys) dir =
  filter (isConfigPositionSat tttC)
  [TTTPosition (xs + ind) (ys + dir * ind) | ind <- [(- w - h) .. (w + h)]]
  where
    (TTTConfig w h _ _) = tttC

getUpDiagPos :: TTTConfig -> Int -> [TTTPosition]
getUpDiagPos tttC diagInd
  | diagInd >= 0 && diagInd < w = getDiagHelp tttC (TTTPosition diagInd 0) (-1)
  | diagInd >= w && diagInd < w + h - 1 = getDiagHelp tttC (TTTPosition (w - 1) (diagInd - w + 1)) (-1)
  | otherwise = []
  where
    (TTTConfig w h _ _) = tttC

getDownDiagPos :: TTTConfig -> Int -> [TTTPosition]
getDownDiagPos tttC diagInd
  | diagInd >= 0 && diagInd < h = getDiagHelp tttC (TTTPosition 0 (h - diagInd - 1)) 1
  | diagInd >= h && diagInd < w + h - 1 = getDiagHelp tttC (TTTPosition (diagInd - h + 1) 0) 1
  | otherwise = []
  where
    (TTTConfig w h _ _) = tttC

getRowInd :: TTTConfig -> [Int]
getRowInd tttC = let h = fieldHeight tttC in [0..h - 1]

getRowPos :: TTTConfig -> Int -> [TTTPosition]
getRowPos tttC rowNum = [TTTPosition x rowNum | x <- [0..fieldWidth tttC - 1]]

getColumnInd :: TTTConfig -> [Int]
getColumnInd tttC = let w = fieldWidth tttC in [0..w - 1]

getColumnPos :: TTTConfig -> Int -> [TTTPosition]
getColumnPos tttC columnNum = [TTTPosition columnNum y | y <- [0..fieldHeight tttC - 1]]

getRowsSmart :: TTTPlay -> (TTTConfig -> [Int]) -> (TTTPlay -> Int -> TTTRow) -> [TTTRow]
getRowsSmart tttPlay indFunc rowFunc = map (rowFunc tttPlay) (indFunc (config tttPlay))

getRowSmart :: TTTPlay -> (TTTConfig -> Int -> [TTTPosition]) -> Int -> TTTRow
getRowSmart tttPlay posFunc rowPos = TTTRow posCell
  where
    posCell = map (calcPosCell tttPlay) (posFunc (config tttPlay) rowPos)

{--
getRowsView tttPlay rowFunc
  | isJust winningRow =
  where
    rows = getRows tttPlay
    winningRow = calcWinningRow tttPlay rowFunc
--}

getRows tttPlay = getRowsSmart tttPlay getRowInd getRow
getColumns tttPlay = getRowsSmart tttPlay getColumnInd getColumn
getUpDiags tttPlay = getRowsSmart tttPlay getDiagInd getUpDiag
getDownDiags tttPlay = getRowsSmart tttPlay getDiagInd getDownDiag

getRow tttPlay = getRowSmart tttPlay getRowPos
getColumn tttPlay = getRowSmart tttPlay getColumnPos
getUpDiag tttPlay = getRowSmart tttPlay getUpDiagPos
getDownDiag tttPlay = getRowSmart tttPlay getDownDiagPos

calcMaxSubSeq :: TTTRow -> TTTRow
calcMaxSubSeq (TTTRow cps) = maxRow filtered
  where
    isW (TTTCellPosition (TTTCellType _) _) = True
    isW _ = False
    filtered = filter (isW . head) $ -- only the ones that are winning
      groupBy (\(TTTCellPosition c1 _) (TTTCellPosition c2 _) -> c1 == c2) cps
    maxRow [] = TTTRow []
    maxRow rs = TTTRow $ maximumBy (compare `on` length) rs

calcTotalFields :: TTTConfig -> Int
calcTotalFields tttC = fieldWidth tttC * fieldHeight tttC

--getWinner :: TTTCellPosition ->

checkRowWinner :: TTTConfig -> TTTRow -> TTTWinner
checkRowWinner tttC cs  | won = TTTWinner ct
                        | otherwise = UnknownWinner
                        where
                          TTTRow subSeq = calcMaxSubSeq cs
                          won = inRow2Win tttC <= length subSeq -- the length of the subsequence exceeds
                          (TTTCellPosition (TTTCellType ct) _) = head subSeq

allRows :: TTTPlay -> [TTTRow]
allRows tttP = concat $ zipWith ($) [getRows, getColumns, getUpDiags, getDownDiags] $ repeat tttP

calcWinningState :: TTTPlay -> (TTTPlay -> [TTTRow]) -> TTTWinner
calcWinningState tttP rowFunc | isJust wr = (snd . fromJust) wr
                              | calcTotalFields cnf == movesCount tttP = Deuce
                              | otherwise = UnknownWinner
                              where
                                cnf = config tttP
                                wr = calcWinningRow tttP rowFunc

calcWinningRow :: TTTPlay -> (TTTPlay -> [TTTRow]) -> Maybe (TTTRow, TTTWinner)
calcWinningRow tttP rowFunc
  | null winningRows = Nothing
  | otherwise = Just (head winningRows)
  where
    cnf = config tttP
    winningRows = filter (isW.snd) $ map (\row -> (row, checkRowWinner cnf row)) (rowFunc tttP)
    isW (TTTWinner _) = True
    isW _ = False


calcAllWinningState tttp = calcWinningState tttp allRows

calcMoves :: TTTPlay -> TTTStepType -> Int
calcMoves tttP tttSt = length $ filter (\t -> stepType t == tttSt) (steps tttP)

isFinishedWinner :: TTTWinner -> Bool
isFinishedWinner (TTTWinner Krestig) = True
isFinishedWinner (TTTWinner Nolek) = True
isFinishedWinner Deuce = True
isFinishedWinner _ = False

isFinished :: TTTPlay -> Bool
isFinished tttP = isFinishedWinner $ calcAllWinningState tttP

calcNextAct :: TTTPlay -> Maybe TTTStepType
calcNextAct tttP  | isFinished tttP = Nothing
                  | stK > stN = Just Nolek
                  | stK < stN = Just Krestig
                  | otherwise = Just $ firstStep $ config tttP
                  where
                    stK = calcMoves tttP Krestig
                    stN = calcMoves tttP Nolek

{-- Parsing functions --}
parseX :: String -> Maybe Int
parseX str  | length str == 1 && isAsciiLower c = Just $ ord c - ord 'a'
            | otherwise = Nothing
            where c = toLower $ head str

parsePosition :: String -> Maybe TTTPosition
parsePosition line  | length vals == 2 = let
                        xm = parseX (vals !! 0)
                        ym = (+ (-1)) <$> readMaybe (vals !! 1) :: Maybe Int
                      in pure TTTPosition <*> xm <*> ym
                    | otherwise = Nothing
                    where g c1 c2 = isDigit c1 == isDigit c2
                          vals = groupBy g line

parseConfig :: String -> Maybe TTTConfig
parseConfig line  | length values == 3 && all isJust values =
                    let [w,h,row] = map fromJust values in
                      Just TTTConfig {
                        fieldWidth = w,
                        fieldHeight = h,
                        inRow2Win = row,
                        firstStep = Krestig
                      }
                  | otherwise = Nothing
                  where values = map (readMaybe :: String -> Maybe Int) (words line)

readUntilGood :: String -> (String -> Maybe a) -> IO a
readUntilGood title parseFun = do
  putStrLn title
  line <- getLine
  let object = parseFun line
  if (isJust object)
    then return $ fromJust object
    else readUntilGood title parseFun

{-- Enf of parsing functions --}


isValidMove :: TTTPlay -> TTTPosition -> Bool
isValidMove play pos =
  isConfigPositionSat (config play) pos && -- the position falls in the right place
  isJust (emptyPositions <$> calcNextStepInfo play >>= find (== pos)) --

calcNextStepInfo :: TTTPlay -> Maybe TTTNextStepInfo
calcNextStepInfo play =
  calcNextAct play >>=
    (\na -> return TTTNextStepInfo{ emptyPositions = validCells, nextStepType = na})
  where (TTTConfig w h _ _) = config play
        validCells = [TTTPosition x y | x <- [0..w - 1], y <- [0..h - 1], calcPos play (TTTPosition x y) == EmptyCell]

calcStep :: TTTPlay -> TTTPosition -> Maybe TTTStep
calcStep play pos | isValidMove play pos = TTTStep pos <$> calcNextAct play
                  | otherwise = Nothing

makeMove :: TTTPlay -> TTTPosition -> TTTPlay
makeMove play pos | isJust mStep = TTTPlay {
                    config = config play,
                    steps = steps play ++ [fromJust mStep]}
                  | otherwise = play
                  where mStep = calcStep play pos

playUntilEnd :: TTTPlay -> (TTTPlay -> Maybe TTTPosition) -> TTTStepType -> IO ()
playUntilEnd play compFunc compStep
  | isJust mNextAct =
    let act = fromJust mNextAct in
      if act == compStep then do
        let newPlay = maybe play (makeMove play) (compFunc play)
        playUntilEnd newPlay compFunc compStep
      else do
        outputPlay play
        pos <- readUntilGood "Input pos" parsePosition
        playUntilEnd (makeMove play pos) compFunc compStep
  | otherwise = do
    outputPlay play
    outputWinner $ calcAllWinningState play
  where mNextAct = calcNextAct play

outputStringColor :: String -> Color -> IO()
outputStringColor str color = do
  setSGR [SetColor Foreground Vivid color]
  putStr str
  setSGR [Reset]

outputStepType :: TTTStepType -> IO ()
outputStepType Krestig = outputStringColor "X" Red
outputStepType Nolek = outputStringColor "O" Green

outputCellType :: TTTCellType -> IO ()
outputCellType (TTTCellType step) = outputStepType step
outputCellType EmptyCell = putStr " "
outputCellType WrongCell = putStr "A"

outputCellPosition :: TTTCellPosition -> IO ()
outputCellPosition (TTTCellPosition ct _ ) = outputCellType ct

outputWinner :: TTTWinner -> IO ()
outputWinner Deuce = putStr "Thanks for playing!" >> outputStringColor "Deuce" Yellow >> putStrLn ""
outputWinner UnknownWinner = outputStringColor "Unknown" Yellow
outputWinner (TTTWinner st) = putStr "Thanks for playing! Winner is " >> outputStepType st >> putStrLn ""

outputConfig :: TTTConfig -> IO ()
outputConfig (TTTConfig w h r start) = do
  putStr $ "[" ++ show w ++ " x " ++ show h ++ "]. " ++
    "Collect " ++ show r ++ " in row to win. "
  outputStepType start
  putStrLn " starts"

outputUpperIndex :: Int -> IO ()
outputUpperIndex columnCount = putStrLn indexes
  where
    indexes = "  " ++ concatMap ((++ " ").(:[])) (take columnCount $ iterate succ 'A')

outputRow :: String -> String -> String -> Int -> IO()
outputRow cs cm ce cc
  | cc >= 1 = putStrLn $ " " ++ cs ++ concat (replicate (cc - 1) cm) ++ ce
  | otherwise = putStrLn ""

outputUpperRow = outputRow "┏" "━┯" "━┓"
outputLowerRow = outputRow "┗" "━┷" "━┛"
outputMiddleRow = outputRow "┠" "─┼" "─┨"

getRowIndex :: TTTCellPosition -> Int
getRowIndex (TTTCellPosition _ (TTTPosition _ y)) = y

outputFieldRow :: TTTRow -> IO()
outputFieldRow (TTTRow row)
  | not (null row) = do
      putStr $ show $ getRowIndex (head row) + 1
      putStr "┃"
      mapM_ (\ttc -> outputCellPosition ttc >> putStr "|") (init row)
      outputCellPosition (last row)
      putStrLn "┃"
  | otherwise = putStrLn ""

outputPlay :: TTTPlay -> IO ()
outputPlay tttPlay = do
  outputUpperIndex w
  outputUpperRow w
  mapM_ (\fr -> outputFieldRow fr >> outputMiddleRow w) (init rows)
  outputFieldRow (last rows)
  outputLowerRow w

  where
    w = (fieldWidth.config) tttPlay
    rows = getRows tttPlay

{-- the solving functions --}
solveFirstEmpty :: TTTPlay -> Maybe TTTPosition
solveFirstEmpty play = (head . emptyPositions) <$> calcNextStepInfo play
{-- end of solving functions --}

main :: IO ()
main = do
  putStrLn "Let's play TicTacToe"
  cnf <- readUntilGood "Input width, height and winning row length: " parseConfig
  putStr "Starting game with config " >> outputConfig cnf
  playUntilEnd (startPlay cnf) solveFirstEmpty Nolek
