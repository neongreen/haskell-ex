{- README

-- Sprial

--- Spec

Given the width of a spiral, draw a spiral. For example:

    > spiral 9

    *********
            *
    ******* *
    *     * *
    * *** * *
    * * * * *
    * *   * *
    * ***** *
    *       *
    *********



-- Problem Solving

    2 4 5 3 1 <---- Step
                       |
            8 <- Size  |
    6     4 |       |  |
    | 2   | |       |  |
    | | 1 | |       |  |
    | | | | |       |  |
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

     !      A    B    C    D    E
(11) 9,8    9,6  7,4  5,2  3,0  1,0

Specifically:

                    - > 9 7 5 3 1 0
                    | > 6 4 2 0 0 0
      !
     -9 *********
                *    |6
                *                -7 *******
                *    *                    *    |2
                *    *                    *            -3 ***
                *    *                    *     *          |0   -1 *
                *    *                    *     *
                *    *                          ***** -5
                *    *                   |4
                     ********* -9
               |8

* H/V lengths shrink by 2 each step
* V length is H - 3
* Negative values are ignored

! NOTE First loop is special case because it doesn't nest (h is smaller by 1)

TODO Given the above pattern there should be a simple calculation to solve this problem...
-}



module Main where

import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix



main :: IO ()
main = do
  putStrLn "How wide do you want the spiral to be?"
  putStrLn . render . spiral . (read :: n -> Int) =<< getLine



spiral :: Int -> Matrix Char
spiral size =
  goRightDown (Matrix.matrix (size + 1) size (const ' '))
  where
  goLeftUp m
    | Matrix.ncols m <= 2 = (drawLineH lr . drawLineV 1) m
    | otherwise           = Matrix.joinBlocks (tl,(goRightDown tr),bl,br)
    where
    (tl,tr,bl,br) = (splitUpRight . draw) m
    draw          = drawPoint (1, 2) . drawLineH lr . drawLineV 1
    lc            = Matrix.ncols m
    lr            = Matrix.nrows m
  goRightDown m
    | Matrix.ncols m <= 2 = (drawLineH 1  . drawLineV lc) m
    | otherwise           = Matrix.joinBlocks (tl,tr,(goLeftUp bl),br)
    where
    (tl,tr,bl,br) = (splitDownLeft . draw) m
    draw          = drawPoint (lr, lc - 1) . drawLineH 1 . drawLineV lc
    lc            = Matrix.ncols m
    lr            = Matrix.nrows m



{- | Take the next sub-matrix for a left-curve.

For example, visually:

                     * * * * * | * *
    * * * * * * *    * * * * * | * *
    * * * * * * *    ----------|----
    * * * * * * *    * * * * * | * *
    * * * * * * *    * * * * * | * *
    * * * * * * *    * * * * * | * *
    * * * * * * *    * * * * * | * *

We carve 2 away: 1 for parent line, 1 for gap
-}
splitDownLeft m = Matrix.splitBlocks 2 (Matrix.ncols m - 2) m

{- | Take the next sub-matrix for a right-curve.

For example, visually:

                     * * | * * * * *
    * * * * * * *    * * | * * * * *
    * * * * * * *    * * | * * * * *
    * * * * * * *    * * | * * * * *
    * * * * * * *    ---------------
    * * * * * * *    * * | * * * * *
    * * * * * * *    * * | * * * * *

We carve 2 away: 1 for parent line, 1 for gap
-}
splitUpRight m  = Matrix.splitBlocks (Matrix.nrows m - 2) 2 m

-- Draw utils
drawPoint = Matrix.setElem '*'
drawLineH = Matrix.mapRow (\_ _ -> '*')
drawLineV = Matrix.mapCol (\_ _ -> '*')



render :: Matrix Char -> String
render = concatMap (++ "\n") . Matrix.toLists
