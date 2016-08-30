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
    print $ "Your move:\n> "
    moveStr <- getLine
    case strToPos moveStr of
        Nothing -> humanTurn board label
        Just pos -> return $ board // [ ( pos, label ) ]

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
        isWinning vec = length ( filter (/= FREE) $ nub $ map ( (!) board ) vec ) == 1

        
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
        scoredBoards    = map ( minimax topLabel $ other curLabel ) availBoards
        minSort         = sortBy ( compare `on` fst ) scoredBoards
        maxSort         = reverse minSort
        sortedBoards    = if topLabel == curLabel then maxSort else minSort

--minimaxTurn :: Turn
--minimaxTurn board label =
--    let x = 1
--        y = 2
--    in return $ minimax MAX board label

play :: Turn -> Turn -> IO ()
play turn1 turn2 = undefined

main = do
    hSetEncoding stdout utf8
    print "Who goes first (human/computer)?"
    opt <- liftM ( map toLower ) $ getLine
    let b = emptyBoard // [ ((1,1), X), ((2,2),O) ]
    putStrLn $ showBoard b