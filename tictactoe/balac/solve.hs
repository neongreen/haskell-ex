import Control.Monad
import System.Random
import System.IO
import Data.Array
import Data.List
import Data.Char

data Label = FREE | O | X
    deriving ( Eq, Show, Enum, Bounded )

type Size = (Int, Int)

type Board = Array Size Label

type Pos = (Int, Int)

type Move = (Pos, Label)

type Turn = Board -> Label -> IO Board

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

humanTurn :: Turn
humanTurn board label = do
    print $ "Your move:\n> "
    moveStr <- getLine
    case strToPos moveStr of
        Nothing -> humanTurn board label
        Just pos -> return $ board // [ ( pos, label ) ] 

play :: Turn -> Turn -> IO ()
play turn1 turn2 = undefined

main = do
    hSetEncoding stdout utf8
    print "Who goes first (human/computer)?"
    opt <- liftM ( map toLower ) $ getLine
    let b = emptyBoard // [ ((1,1), X), ((2,2),O) ]
    putStrLn $ showBoard b