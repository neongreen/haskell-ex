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
import Criterion.Main
import Data.Char (chr, ord)
import Data.ByteString.UTF8



bigTree = expoAlphaTree 15

main :: IO ()
main = defaultMain [
    bgroup "show+UTF8" [
       bench "expoAlphaTree (^15)" $ nf (fromString . show) bigTree
    ]
  ]



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
  unfoldTree go 'A'
  where
  end = chr (ord 'A' + (count - 1))
  go letter
    | letter >= end = ([letter], [])
    | otherwise     = ([letter], replicate 2 (succ letter))



-- Development --

demoTree :: Int -> IO ()
demoTree = putStrLn . drawTree . expoAlphaTree
