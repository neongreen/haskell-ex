module Main where

import Control.Monad
import Draw

main :: IO ()
main = do
  drawBoard
  promptMove

promptMove :: IO()
promptMove = do
  pos <- drawPrompt
  case convertStrToPosition pos of
    PositionUnsupported _ ->
      promptMove
    pos -> do
      drawToken Cross pos
      promptMove
