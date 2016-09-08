module Main where

import Control.Monad
import Draw

main :: IO ()
main = do
  drawBoard
  putStrLn "You move: "
  pos <- getLine
  case convertStrToPosition pos of
    PositionUnsupported message -> do
      putStrLn ("ERROR. " ++ message)
      getLine
      main
    _ ->
      putStrLn pos
