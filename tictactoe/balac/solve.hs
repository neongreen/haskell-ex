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

bSize = 3

emptyBoard :: Board
emptyBoard = array boardRange [ ( idx, FREE ) | idx <- range boardRange ]
    where
        boardRange = ( ( 1, 1 ), ( bSize, bSize ) )

showBoard board = unlines $ headerRow ++ intersperse sepRow ( map showRow rows ) ++ footerRow
    where
        headerRow = [ xLabelRow, topBoxRow ]
        xLabelRow = "  " ++ ( intersperse ' ' $ take bSize [ 'A'..'Z' ] )
        topBoxRow = " ┏" ++ ( intersperse '┯' $ take bSize $ repeat '━' ) ++ "┓"
        footerRow = [ " ┗" ++ ( intersperse '┷' $ take bSize $ repeat '━' ) ++ "┛" ]
        sepRow    = " ┠" ++ ( intersperse '┼' $ take bSize $ repeat '━' ) ++ "┨"
        showRow ( rowIdx, rowData ) = ( show rowIdx ) ++ ( '┃' : intersperse '│' ( map lookupLabel rowData ) ++ ['┃'] )
        rows = map (\i -> ( i, [ board ! (i,j) | j <- [1..bSize] ] ) ) [1..bSize]
        lookupLabel label = case label of
                                FREE -> ' '
                                O    -> 'O'
                                X    -> 'X'

main = do
    hSetEncoding stdout utf8
    print "Who goes first (human/computer)?"
    opt <- liftM ( map toLower ) $ getLine
    let b = emptyBoard // [ ((1,1), X), ((2,2),O) ]
    putStrLn $ showBoard b