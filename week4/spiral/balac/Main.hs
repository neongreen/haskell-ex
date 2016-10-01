import System.IO
import Data.Array

data Dir = DOWN | LEFT | UP | RIGHT
                deriving( Show, Ord, Eq, Enum )

move :: (Int,Int) -> Dir -> (Int,Int)
move (x,y) d = case d of
    DOWN -> (x, y+1)
    LEFT -> (x-1, y)
    UP   -> (x, y-1)
    RIGHT-> (x+1,y)

spiralDirs :: Int -> [Dir]
spiralDirs width = concatMap (\(num, dir) -> replicate num dir ) $ ( width-1, RIGHT ) : zip ( reverse [1..width] ) ( cycle [ DOWN .. RIGHT ] )

spiralIndices :: Int -> [(Int,Int)]
spiralIndices width = scanl move (1,1) $ spiralDirs width

showSpiral :: Array (Int,Int) Char -> String
showSpiral spiral = unlines rows
                    where
                        (width,height) = snd $ bounds spiral 
                        rows = map (\row -> [ spiral ! ( col, row ) | col <- [ 1 .. width ] ] ) [ 1 .. height ] 

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    putStr "Size? "
    width <- read <$> getLine
    let indices = spiralIndices width
        arr = listArray ( (1,1), (width, width+1) ) ( repeat ' ' )
        spiral = arr // [ ( ind, '*' ) | ind <- indices ]
    putStrLn $ showSpiral spiral