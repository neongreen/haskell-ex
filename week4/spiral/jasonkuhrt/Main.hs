{- README

-- Sprial

--- Spec

Given the width of a spiral, draw a spiral. For example:

    > spiral 9

    2 4 5 3 1

            8
    6     4 |
    | 2   | |
    | | 1 | |
    | | | | |
    ********* ----- 9  1
          B *
    ******* * ---- 7   3
    *   D * *
    * *** * * -- 3     5
    * * * * *
    * * E * *
    * ***** * --- 5    4
    * C     *
    ********* ----- 9  2
    A

How do we calulate verticals?  horizontal - 3
e.g.

A    B    C    D    E
9,6  7,4  5,2  3,0  1,0

NOTE First loop is special case because it doesn't nest (h is smaller by 1)

in-out, per loop
v grows by 2
h grows by 2

out-in, per loop
v shrinks by 2
h shrinks by 2

total loops for n: div n 2    e.g. div 9 2 = 4 (discard remainder)

V prev size      +0  +4  +6
gap size         +1  +1  +1
H bar size       +2  +1  +1
              1 > 4 > 6 > 8

    > spiral 4

    ****
       *
    ** *
    *  *
    ****
-}

module Main where

import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix



main :: IO ()
main = putStrLn . render . spiral $ 9



spiral :: Int -> Matrix Char
spiral n =
  Matrix.mapRow (\_ _ -> '*') 1 .
  Matrix.mapCol (\_ _ -> '*') n $
  matrix
  where
  matrix = Matrix.matrix (n - 1) n (const ' ')



render :: Matrix Char -> String
render = concatMap (++ "\n") . Matrix.toLists
