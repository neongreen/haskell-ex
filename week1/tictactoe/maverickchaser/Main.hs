import qualified Data.Map as Map
import System.Console.ANSI
import Control.Monad
import Data.Maybe

type Cell = (Int, Int)
type Board = Map.Map Cell Player
data OutCome = Wins Player | Draw | Unknown deriving (Eq, Show)
type Triple  = [Cell]
-- type Move    = (Cell, Move)
data Player = X | O | Empty deriving (Eq, Show)

empty :: Cell -> Board -> Bool
empty c b = isNothing (Map.lookup c b)

emptyCells :: Board -> [Cell]
emptyCells b =
    let cells = [(col, row) | row <- [1..3], col <- [1..3]]
    in
    [cell | cell <- cells, empty cell b]

checkTriple :: Triple -> Board -> Player -> Bool
checkTriple triple b val =
    all (\cell -> Map.lookup cell b == Just val) triple

checkTriples :: [Triple] -> Board -> Player -> Bool
checkTriples triples b val =
    all (\triple -> checkTriple triple b val) triples

checkCols :: (Board -> Player -> Bool)
checkCols =
    let triples = [[(col, row) | row <- [1..3]] | col <- [1..3]]
    in
        checkTriples triples

checkRows :: (Board -> Player -> Bool)
checkRows =
    let triples = [[(col, row) | col <- [1..3]] | row <- [1..3]]
    in
        checkTriples triples

checkDiag :: (Board -> Player -> Bool)
checkDiag =
    let triples = [[(1, 1), (2, 2), (3, 3)], [(3, 1), (2, 2), (1, 3)]]
    in
        checkTriples triples

checkAll :: Board -> Player -> Bool
checkAll b player =
    checkCols b player || checkRows b player || checkDiag b player

outCome :: Board -> OutCome
outCome b
    | checkAll b X        = Wins X
    | checkAll b O        = Wins O
    | null b              = Draw
    | otherwise           = Unknown

applyMove :: Board -> Player -> Cell -> Board
applyMove b player c =
    Map.insert c player b

opponent :: Player -> Player
opponent player
    | player == X = O
    | player == O = X

brute :: Board -> Player -> (OutCome, Maybe Cell)
brute b player =
    let outcome = outCome b
    in
    if outcome == Unknown
        then
            let cells    = emptyCells b
                opp      = opponent player
                outcomes = map (\cell -> fst $ brute (applyMove b player cell) opp) cells
                moves    = zip outcomes (map Just cells)
                winning  = filter (\move -> fst move == Wins player) moves
                draw     = filter (\move -> fst move == Draw) moves
                losing   = filter (\move -> fst move == Wins opp) moves
            in
                if not $ null winning
                    then head winning
                    else if not $ null draw
                        then head draw
                        else
                            head losing
        else
            (outcome, Nothing)

printBoard :: Board -> IO ()
printBoard b = do
            putStrLn "  A B C"
            putStrLn " ┏━┯━┯━┓"
            cursorDown 1
            forM_ [1..3] $ \rowNum -> do
                let players = map (\colNum -> case Map.lookup (colNum, rowNum) b of
                                                Just val -> show val
                                                Nothing  -> " "
                                    ) [1..3]
                let line = show rowNum ++ foldl (\acc val -> acc ++ "┃" ++ val) "" players ++ "┃"
                putStrLn line
                if rowNum < 3
                    then putStrLn " ┠─┼─┼─┨"
                    else putStr ""

            putStrLn " ┗━┷━┷━┛"

readPair :: IO (Int, Int)
readPair = do
        [a,b] <- fmap (map read . words) getLine
        return (a,b)

runGame :: Board -> IO ()
runGame b = do
    printBoard b
    putStrLn "Your move"
    (col,row) <- readPair
    let nb = applyMove b X (col,row)
    let outcome = outCome nb
    if  outcome == Unknown
        then do
            let (_, Just move) = brute nb O
                nnb = applyMove nb O move
                outcome = outCome nnb
            if outcome == Unknown
                then runGame nnb
                else do
                    printBoard nnb
                    print outcome
        else do
            printBoard nb
            print outcome

main = do
    putStrLn "Starting the game.."
    let b = Map.fromList []
    runGame b
