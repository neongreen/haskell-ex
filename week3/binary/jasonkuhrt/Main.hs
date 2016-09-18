{- #  Binary Conversion

  ##  Spec

      Convert a positive integer to binary and back.
      Ignore negative numbers and fractions.

  ##  Examples

      > toBinary 123
      "1111011"

      > toDecimal "1111011"
      123

  ##  What is a binary number?

      > In mathematics and digital electronics, a binary number is a number
      expressed in the binary numeral system or base-2 numeral system which
      represents numeric values using two different symbols: typically 0
      (zero) and 1 (one). The base-2 system is a positional notation with a
      radix of 2. Because of its straightforward implementation in digital
      electronic circuitry using logic gates, the binary system is used
      internally by almost all modern computers and computer-based devices.
      Each digit is referred to as a bit.
-}

import Test.QuickCheck ((===))
import qualified Test.QuickCheck as QS

type Binary = String



main :: IO ()
main =
  (QS.quickCheck . QS.property) identityProp
  where
  identityProp (QS.NonNegative n) =
    n === (toDecimal . toBinary) n



{-
Convert binary into decimal. For example:

    > toDecimal "101"
    5

The following table shows initial increments
and overflow in binary counting.

    DEC    BIN     POW

    0         0    2^0 (1)
    1         1
    2        10    2^1 (2)
    3        11
    4       100    2^2 (4)
    5       101
    6       110
    7       111
    8      1000    2^3 (8)
    .      ....

Our solution is derived from noting the relationship
between exponetals and significant digits. For example
in 1000b there are three significant digits after "1". So the
value of "1" can be ascertained by raising it to the power of
three. So generally: The value of a binary digit can be found by
raising it to the power of the number of trailing significant
digits.
-}

toDecimal :: Binary -> Integer
toDecimal =
  foldl addUp 0 . zip [0..] . reverse
  where
  addUp val (sigDig, '0') = val
  addUp val (sigDig, '1') = val + 2^sigDig

{- Feedback

I was shown simpler/more concise ways to implement this.

1.  Array comprehensions + pattern match
    via @neongreen

    This solution leverages array comprehension (1) syntax.
    The pattern matching used to draw values out (2) filters
    out data that we don't need: significant digits of digit 0
    which cotribute zero value to the final sum!

        sum [2^sigDig | (sigDig, '1') <- zip [0..] (reverse xs)]
            ^1          ^2

2.  Math AKA "correct" engineering solution
    via @DL

    I love this but given my knowledge/way of thinking I do not think
    it would be within ready grasp to arrive at this solution. It
    would require at least understanding the correspondance between
    powers of the significant digit versus repeated value doubling (1)
    plus one or zero (2) neigther of which seem like the kind of insight
    I would strike upon. If this was the _only_ way to solve the problem
    I assume that I would eventually arrive at it, though :).

        foldl' (\val flag -> val * 2 + digitToInt flag) 0
                                 ^1  ^2
-}




{-
Convert decimal into binary. For examle:

    > toBinary 10
    "1010"

Repeatedly halve an integer until 0. The remainder of
each division will be used in the result as the
*next-least-significant* (NLS) bit.

For example:

    Values        10 -> 5 -> 2 -> 1 -> 0
    Remainders          0    1    0    1

Remembering that each reminder is an NLS bit we just
reverse them to get the correct result:

    1010
-}

toBinary :: Integer -> Binary
toBinary = concatMap show . reverse . go . divModHalf
  where
  go (0, remainder) = [remainder]
  go (n, remainder) = remainder : go (divModHalf n)



-- General Helpers --

divModHalf = (`divMod` 2)
