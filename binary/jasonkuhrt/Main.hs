
{- #  Binary Conversion
      Convert an integer to binary and back.

  ##  Example

      > bin 123
      "1111011"

      > dec "1111011"
      123

  ##  What is a binary number?

      > In mathematics and digital electronics, a binary number is a number
      expressed in the binary numeral system or base-2 numeral system which
      represents numeric values using two different symbols: typically 0 (zero)
      and 1 (one). The base-2 system is a positional notation with a radix of
      2. Because of its straightforward implementation in digital electronic
      circuitry using logic gates, the binary system is used internally by
      almost all modern computers and computer-based devices. Each digit is
      referred to as a bit.

  ##  Figure 1

      Table showing initial increments and
      overflow in binary counting.

      DEC    BIN     POW

      0      0000    2^0 (1)
      1      0001
      2      0010    2^1 (2)
      3      0011
      4      0100    2^2 (4)
      5      0101
      6      0110
      7      0111
      8      1000    2^3 (8)
      .      ....

-}




module Main where

import qualified Test.QuickCheck as QS
import qualified Data.List as List



type Binary = Integer
type Digit = Integer



main :: IO ()
main = undefined



toDecimal :: Binary -> Integer
toDecimal =
  foldl addUp 0 . zip [0..] . reverse . digits
  where
  addUp val (sigDig, 0) = val
  addUp val (sigDig, 1) = val + 2^sigDig



toBinary :: Integer -> Binary
toBinary n = foldl1 (\acc _ -> binNext acc) [0..n]

-- If there are zeros
--  Find right-most
--  flip it (to 1)
--  everything else right, flip it (to 0)
-- otherwise (AKA overflow), prepend 1
--   everything else right, flip it (to 0)
binNext :: Binary -> Binary
binNext = undigits . go . digits
  where
  go binum
    | null zeros = 1 : fmap bitFlip binum
    | otherwise  =
      let (remain, toInvert) = splitAt (last zeros) binum in
      remain ++ fmap bitFlip toInvert
    where
    zeros = List.elemIndices 0 binum
    bitFlip 1 = 0
    bitFlip 0 = 1




-- General Helpers --

digits :: Integer -> [Digit]
digits = fmap (read . (:[])) . show

undigits :: [Digit] -> Integer
undigits = read . concatMap show
