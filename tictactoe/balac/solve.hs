import Control.Monad
import System.Random
import System.IO
import Data.Array
import Data.List
import Data.Char
import Data.Maybe
import Data.Function

data Label = FREE | O | X
    deriving ( Eq, Show, Enum, Bounded )

type Size = (Int, Int)

type Board = Array Size Label

type Pos = (Int, Int)

type Move = (Pos, Label)

type Turn = Board -> Label -> IO Board

type ScoredBoard = (Int, Board)

data MinimaxTurn = MAX | MIN

data BoardStatus = WIN | DRAW | PLAY

other :: Label -> Label
other O = X
other X = O
other FREE = FREE

bSize = 3

emptyBoard :: Board
emptyBoard = array boardRange [ ( idx, FREE ) | idx <- range boardRange ]
    where
        boardRange = ( ( 1, 1 ), ( bSize, bSize ) )

showBoard board = unlines $ headerRows ++ intersperse sepRow ( map showRow rows ) ++ [ footerRow ]
    where
        headerRows= [ xLabelRow, topBoxRow ]
        xLabelRow = "  " ++ ( intersperse ' ' $ take bSize [ 'A'..'Z' ] )
        topBoxRow = borderRow '┏' '┯' '┓'
        sepRow    = borderRow '┠' '┼' '┨'
        footerRow = borderRow '┗' '┷' '┛'
        borderRow start middle end = [ ' ', start ] ++ ( intersperse middle $ take bSize $ repeat '━' ) ++ [ end ]
        showRow ( rowIdx, rowData ) = ( show rowIdx ) ++ ( '┃' : intersperse '│' ( map lookupLabel rowData ) ++ ['┃'] )
        rows = map (\i -> ( i, [ board ! (i,j) | j <- [1..bSize] ] ) ) [1..bSize]
        lookupLabel label = case label of
                                FREE -> ' '
                                O    -> 'O'
                                X    -> 'X'

strToPos :: String -> Maybe Pos
strToPos str = do
    if length str' < 2
        then Nothing
        else do
            row <- getRow
            col <- getCol
            if all ( \x -> x > 0 && x <= bSize ) [ row, col ]
                then return (row, col)
                else Nothing
    where
        str'   = trim $ map toLower str
        trim   = unwords . words
        getCol = Just ( ord ( str' !! 0 ) - 96 )
        rowStr = tail str'
        getRow = if all isDigit rowStr
                    then Just ( read rowStr :: Int )
                    else Nothing

applyMove :: Board -> Move -> Board
applyMove board move = board // [ move ]

humanTurn :: Turn
humanTurn board label = do
    putStr "Your move:\n> "
    moveStr <- getLine
    case strToPos moveStr of
        Nothing -> humanTurn board label
        Just pos -> return $ applyMove board ( pos, label )

winVectors :: [ [ Pos ] ]
winVectors = rows ++ cols ++ diagonals
    where
        idxs = [1..bSize]
        rows = map (\row -> map (\col -> ( row, col ) ) idxs ) idxs
        cols = map (\col -> map (\row -> ( row, col ) ) idxs ) idxs
        diagonals = [ map (\idx -> (idx, idx) ) idxs, zip idxs $ reverse idxs ] 


getWinner :: Board -> Maybe Label
getWinner board 
    | length winningVectors == 0 = Nothing
    | otherwise = Just ( board ! ( ( winningVectors !! 0 ) !! 0 ) )
    where
        winningVectors = filter isWinning winVectors
        isWinning vec
            | length uniq /= 1 = False
            | otherwise = ( uniq !! 0 ) /= FREE
            where
                uniq = nub $ map ( (!) board ) vec

        
minimax :: Label -> Label -> Board -> ScoredBoard
minimax topLabel curLabel board
    | hasWon    = if isTopWinning then ( 1, board ) else ( -1, board )
    | isDraw    = ( 0, board )
    | otherwise = head $ sortedBoards
    where
        hasWon          = isJust posWinner
        posWinner       = getWinner board
        winLabel        = fromJust posWinner
        isTopWinning    = winLabel == topLabel
        isDraw          = null availMoves
        availMoves      = [ ( pos, curLabel ) | ( pos, l ) <- assocs board, l == FREE ]
        availBoards     = map ( applyMove board ) availMoves
        scoredBoards'   = map ( minimax topLabel $ other curLabel ) availBoards
        scoredBoards    = zipWith ( \( score, _ ) board -> (score, board) ) scoredBoards' availBoards
        minSort         = sortBy ( compare `on` fst ) scoredBoards
        maxSort         = reverse minSort
        sortedBoards    = if topLabel == curLabel then maxSort else minSort

minimaxTurn :: Turn
minimaxTurn board label = return $ snd $ minimax label label board

winGame :: Board -> IO ()
winGame board = do
    putStrLn $ showBoard board
    putStrLn $ ( "Match Won by " ++ ) $ show . fromJust . getWinner $ board

drawGame :: Board -> IO ()
drawGame board = do
    putStrLn $ showBoard board
    putStrLn "Match Drawn" 

isDrawn :: Board -> Bool
isDrawn = null . filter ( ( FREE == ) . snd ) . assocs

hasWon :: Board -> Bool
hasWon = isJust . getWinner

boardStatus :: Board -> BoardStatus
boardStatus board
    | isDrawn board = DRAW
    | hasWon board  = WIN
    | otherwise     = PLAY

play :: Board -> Label -> Turn -> Turn -> IO ()
play board label turn1 turn2 = do
    putStrLn $ showBoard board
    newBoard <- turn1 board label
    case boardStatus newBoard of
        WIN  -> winGame newBoard
        DRAW -> drawGame newBoard
        PLAY -> play newBoard ( other label ) turn2 turn1

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    putStrLn "Who goes first (human/computer)?"
    opt <- liftM ( map toLower ) $ getLine
    if opt == "h"
        then play emptyBoard X humanTurn minimaxTurn
        else play emptyBoard X minimaxTurn humanTurn