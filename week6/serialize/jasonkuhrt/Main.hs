{- README

Benchmark the performance of different serializers serializing a large tree suitable for then being written to disk.

-- Spec

* Use `criterion` to benchmark.
* Try serializers such as:
  * `show` then convert to UTF-8
  * `aeson` to convert to JSON
  * `binary`
  * `cereal`
  * `binary-serialise-cbor`
  * `MessagePack`

-- Reminders

* Don't forget that performance-critical code should be compiled with -O2.
* Also, don't accidentally benchmark writing into the file itself.
-}
module Main where

import Data.Tree
import Data.Char (chr, ord)



main :: IO ()
main = putStrLn . drawTree $ expoAlphaTree 3


{- An exponential alphabet tree.

 Scale `count` to affect the expoent.

For example given a `count` of 3:

  > expoAlphaTree 3

  A
  |
  +- B
  |  |
  |  +- C
  |  |  |
  |  |  +- D
  |  |  |
  |  |  `- D
  |  |
  |  `- C
  |     |
  |     +- D
  |     |
  |     `- D
  |
  `- B
     |
     +- C
     |  |
     |  +- D
     |  |
     |  `- D
     |
     `- C
        |
        +- D
        |
        `- D

-}
expoAlphaTree :: Int -> Tree String
expoAlphaTree count =
  unfoldTree go start
  where
  start = ord 'A'
  end   = start + count
  go n
    | n >= end     = ([chr n], [])
    | otherwise    = ([chr n], replicate 2 (n + 1))
