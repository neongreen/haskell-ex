module Draw (drawGraphityConsole) where

import System.Console.ANSI

{-
DRAW SECTION

Responses for drawing on console and I/O operations.

Functions:

1. drawBattlefield. Draws game area, where player will see game's result.

      1    2  3
     --- --- ---
  A   x       0
  B       x
  C   x       0
     --- --- ---

   You move:
   > B2

2. drawTestMode. Draws prompt

-}



drawGraphityConsole :: IO ()
drawGraphityConsole = do
  setCursorPosition 0 5
  setTitle "Tictac game 0.1.0"

  clearScreen
  setSGR [
           SetColor Foreground Vivid Red
         ]
  putStr "Hello"

  setSGR [
           SetColor Foreground Vivid White
         ]
  putStrLn "World!"
  putStrLn "You move:"
  putStr "> "
  ch <- getLine
  putStrLn ("You say:" ++ ch)
