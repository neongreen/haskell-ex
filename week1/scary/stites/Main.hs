---------------------------------------------------------------------------------
-- Scary words
--
-- If you assign numbers to letters (A=1, B=2, ..., Z=26), then a word is scary
-- if the sum of its letters is 13. “baaed”, for instance, is scary (especially
-- when at first you don't understand it's a silly verb and think it's an ancient
-- god's name).
--
-- Find all scary words in the `words` file (it's usually in
-- `/usr/share/dict/words` or `/usr/dict/words`).
--
-- Common mistakes:
--
-- * Treating `zip's` as scary (the mistake is in assigning a numbers to *all*
-- characters, not just non-letter ones, and then `'` usually gets a negative
-- code).
--
-- * Treating `Iraq` as scary (uppercase characters should be treated the same as
-- lowercase ones).
---------------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Main where

import Data.Char
import Data.List
import qualified Data.HashSet as H
import qualified Data.Vector as V

main :: IO ()
main = do
  file <- readFile "../../../data/words"
  putStrLn "Scary words:"
  putStrLn . intercalate "," . filter isScary . lines $ file
  return ()


isScary :: [Char] -> Bool
isScary = go 0 False . filter lCharsOnly . map toLower
  where
    lCharsOnly :: Char -> Bool
    lCharsOnly c = H.member c ['a'..'z']

    go :: Int -> Bool -> [Char] -> Bool
    go count scary     [] = scary
    go count scary (c:cs)
      | nxtCount > 13 = False
      | nxtCount == 13 = go nxtCount True cs
      | nxtCount < 13 = go nxtCount scary cs
      where
        nxtCount :: Int
        nxtCount = count + fromEnum c - 96

---------------------------------------------------------------------------------
-- Tests

truelyScary :: IO ()
truelyScary = do
  print $ isScary "baaed"
  print $ isScary $ replicate 13 'a'
  print $ isScary $ '\'':(replicate 13 'a')
  print $ isScary "baaed'''''"

falselyScary :: IO ()
falselyScary = do
  print $ isScary "baaed'''''Z"
  print $ isScary "zip's"
  print $ isScary "foo"

scarycheck :: IO ()
scarycheck = do
  let scares = V.fromList $ map (isScary . (:[])) ['a'..'z']
  putStrLn $ "13th char is " ++ printScariness (scares V.! 12)
  putStrLn $ "all other chars are " ++ printScariness (not13th scares)
  where
    printScariness :: Bool -> [Char]
    printScariness True = "scary"
    printScariness False = "not scary"

    not13th :: V.Vector Bool -> Bool
    not13th = V.all not . V.ifilter (\i _ -> i == 12)


