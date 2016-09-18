module Main where

import System.Console.ANSI
import Control.Monad

-- terminal control functions

resetSGR :: IO ()
resetSGR = setSGR [Reset]

setColor :: Color -> IO ()
setColor c = setSGR [SetColor Foreground Vivid c]

resetScreen :: IO ()
resetScreen = clearScreen >> resetSGR >> setCursorPosition 0 0

-- custom game representation

data Cell = Empty
          | X
          | O

showCell :: Cell -> IO ()
showCell c = do case c of
                  X -> setColor Green >> putStr "X"
                  O -> setColor Red   >> putStr "O"
                  _ -> return ()
                resetSGR

type Grid = [[Cell]]

emptyGrid :: Grid
emptyGrid = [ [Empty, Empty, Empty]
            , [Empty, Empty, Empty]
            , [Empty, Empty, Empty] ]

-- main stuff

showGrid :: Grid -> IO ()
showGrid g = showFrame

showFrame :: IO ()
showFrame = do resetScreen
               mapM_ putStrLn theFrame

setCursorWithinGrid :: Int -> Int -> IO ()
setCursorWithinGrid r c = setCursorPosition (2*r) (2*c)

setCursorOnStatus :: IO ()
setCursorOnStatus = setCursorPosition 9 0

setCursorOnInput :: IO ()
setCursorOnInput = setCursorPosition 11 0

mainLoop :: Grid -> IO Grid
mainLoop g = do
  showFrame
  setCursorWithinGrid 1 1
  showCell O
  setCursorOnStatus
  putStr "Your move"
  setCursorOnInput
  colC <- getChar
  rowC <- getChar
  return g

main :: IO ()
main = do setTitle "h3t"
          mainLoop emptyGrid >>= mainLoop -- and so on
          return ()

theFrame:: [String]
theFrame = [ "  A B C"
           , " ┏━┯━┯━┓"
           , "1┃ | | ┃"
           , " ┠─┼─┼─┨"
           , "2┃ | | ┃"
           , " ┠─┼─┼─┨"
           , "3┃ | | ┃"
           , " ┗━┷━┷━┛" ]
