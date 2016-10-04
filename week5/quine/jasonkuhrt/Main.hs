{- README

-- Code Kata: Quine

Write a quine: a program that prints its own source code.
Tricks like reading your own source with readFile aren't allowed.
-}

module Main where

import qualified Data.List as List

main :: IO ()
main = putStrLn source

source :: String
source = List.intercalate "\n" [
    "module Main where",
    "",
    "import qualified Data.List as List",
    "",
    "main :: IO ()",
    "main = putStrLn source\n",
    "",
    "source :: String",
    "source = List.intercalate \"\\n\" [",
    source,
    "]"
  ]
