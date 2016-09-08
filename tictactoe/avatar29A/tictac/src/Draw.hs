module Draw (BoardToken (..), BoardPosition (..),
             drawBoard,
             drawToken,
             drawPrompt,
             drawError,
             convertStrToPosition) where

import System.Console.ANSI
import Control.Monad
import Data.Char


data BoardPosition = PositionUnsupported String | Position Char Int deriving (Show)

data BoardToken = Cross | Toe

instance Show BoardToken where
  show Cross = "X"
  show Toe = "O"

type AxisBounds = (Char, Char)

-- drawBoard draws a game board
drawBoard :: IO()
drawBoard = do
  clearScreen

  drawXYAxis

  {-Borders-}
  drawTopBorder
  drawBottomBorder
  drawLeftBorder
  drawRightBorder

  drawYBorder 4 6
  drawYBorder 6 6

  drawXBorder 3 8
  drawXBorder 5 8

  setPromptCursorPosition

{- Draw borders -}

drawTopBorder :: IO()
drawTopBorder = do
  setCursorPosition 1 1
  drawWhiteText "╔═══════╗"

drawBottomBorder :: IO()
drawBottomBorder = do
  setCursorPosition 7 1
  drawWhiteText "╚═══════╝"

drawLeftBorder :: IO()
drawLeftBorder = drawYBorder columnIdx length
  where
    columnIdx = 1
    length = 6

drawRightBorder :: IO()
drawRightBorder = drawYBorder columnIdx length
  where
    columnIdx = 9
    length = 6

drawYBorder :: Int -> Int -> IO()
drawYBorder column len =
  forM_ [2..len] (flip (drawLine "║") column)

drawXBorder :: Int -> Int -> IO()
drawXBorder row len =
  forM_ [2..len] (drawLine "═" row)

drawLine :: String -> Int -> Int -> IO()
drawLine ch row column = do
  setCursorPosition row column
  drawWhiteText ch

{--}

{- Draw axis. Y: [A,B,C]  and X: [1, 2, 3] -}

drawXYAxis :: IO ()
drawXYAxis = do
  drawAxis ["1", "2", "3"] cursorForward
  drawAxis ["A", "B", "C"] cursorDownLine

drawAxis :: [String] -> (Int -> IO ()) -> IO()
drawAxis labels padding = do
  setDefaultCursorPosition
  forM_ labels (\a -> do
      padding 2
      drawWhiteText a
    )
 {--}

-- drawToken draws X or O by the given coords
drawToken :: BoardToken -> BoardPosition -> IO()
drawToken token pos = do
  setCursorPosition (positionY pos) (positionX pos)
  drawWhiteText . show $ token
  where
    positionX :: BoardPosition -> Int
    positionX (Position _ 1) = 2
    positionX (Position _ 2) = 5
    positionX (Position _ 3) = 8

    positionY :: BoardPosition -> Int
    positionY (Position 'A' _) = 2
    positionY (Position 'B' _) = 4
    positionY (Position 'C' _) = 6

drawPrompt :: IO String
drawPrompt = do setPromptCursorPosition
                putStrLn "You move: "
                clearFromCursorToScreenEnd
                getLine

drawError :: String -> IO()
drawError message = do
  setPromptCursorPosition
  clearFromCursorToScreenEnd
  drawRedText message

-- drawColorText draws text with was choosen color foreground
drawColorText :: Color -> String -> IO()
drawColorText c message = do
  setSGR [
            SetColor Foreground Vivid c
         ]

  putStr message

-- drawRedText draws text on console in 'red' foreground
drawRedText :: String -> IO()
drawRedText = drawColorText Red

-- drawWhiteText draws text on console in 'white' foreground
drawWhiteText :: String -> IO()
drawWhiteText = drawColorText White

-- drawGreenText draws text on console in 'green' foreground
drawGreenText :: String -> IO()
drawGreenText = drawColorText Green

-- Cursor functions
setDefaultCursorPosition :: IO()
setDefaultCursorPosition = setCursorPosition 0 0

setPromptCursorPosition :: IO()
setPromptCursorPosition = setCursorPosition 10 0

-- reset clear and discard all changes (color, foreground, etc)
reset :: IO()
reset = setSGR [Reset]

convertStrToPosition :: String -> BoardPosition
convertStrToPosition [y, x]
  | isX x && isY y =
    Position y (digitToInt x)
  | otherwise = PositionUnsupported unsupportedErrorMessage
  where
    isX :: Char -> Bool
    isX ch = checkInput ch ('1', '3')

    isY :: Char -> Bool
    isY ch = checkInput ch ('A', 'C')

    checkInput :: Char -> AxisBounds -> Bool
    checkInput ch (from, to) =
      let code = ord ch
      in
        (code >= ord from) && (code <= ord to)

convertStrToPosition _ =
    PositionUnsupported unsupportedErrorMessage

unsupportedErrorMessage :: String
unsupportedErrorMessage = "Expected {NM}, where N in ['A', 'B', 'C'] and M in ['1', '2', '3'], for instance A1, B3, etc."

-- ln adds to endline - '\n'
ln :: String -> String
ln s = s ++ "\n"
