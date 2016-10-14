{-# LANGUAGE FlexibleInstances #-}

{- TODO


* Multi-param functions

* Shrinking

* Count exceptions as failures too.
-}

{- README

-- Code Kata: Quick Check

-- Spec

* Write a `check` function.

  It should:

  * generate arbitrary test cases
  * pass case to a property
  * should a property fail, stop, print the test case

  For example:

  ```
  allNumbersDivideEachOther :: (Int, Int) -> Bool
  allNumbersDivideEachOther (a, b) = a `mod` b == 0
  ```
  ```
  > check allNumbersDivideEachOther
  False! Testcase: (5,3)
  ```

-- Hints

* A custom typeclass for generating random test cases for various types

-- Bonus

* Upgrade `check` to support multi-parameter functions. Hint: Another custom typeclass will be required (e.g. `Testable`).
* Count exceptions as test failures too.
* Add a `shrink` method to your custom typeclass.

  It should:

  * If a property fails a test case, it should try to find a "smaller" tese case that still fails. The motivation for `shrink` come from the observation that a testcase like `(3,2)` is usually nicer for a human to work with than `(13426634,234623)`.

  For example:

  * decrease the number of an `Int`
  * remove elements from a `[Bool]`

-}
module Main where

import System.Random
import Data.Char (chr, ord)
import Control.Monad (replicateM, (>=>))
import Data.List (elemIndex)



main :: IO ()
main = do
  check zeroIsZero
  foobar zeroIsZero
  foobar additionIsCommuntative
  foobar additionIsAssociative

zeroIsZero :: Int -> Bool
zeroIsZero n =
  n == n + 0

additionIsCommuntative :: Int -> Int -> Bool
additionIsCommuntative n o =
  n + o == o + n

additionIsAssociative :: Int -> Int -> Int -> Bool
additionIsAssociative n o v =
  n + (o + v) == (n + o) + v

foobar :: Testable f => f -> IO ()
foobar = test >=> print





-- Library --

{- | Check that given assertion holds for all randomly generated `a`s.

  Input is an assertion; Its:

    Input is polymorphic (at this level); It is the testcase supplied by `check`.

    Output is a `Bool`; `False` represents a failed assertion, that is the assertion did not hold for the corresponding input (AKA testcase).

  Output is printed report stating if and what testcase failed.
-}
check :: (Show a, Arbitrary a) => (a -> Bool) -> IO ()
check f = do
  testCases <- replicateM 100 arbitrary
  let testResults = fmap f testCases
  case elemIndex False testResults of
    Nothing -> putStrLn "==> PASS! 100 test cases passed."
    Just i  -> do
      putStrLn "==> FAIL! Invariant broke with the following input:"
      print . (!! i) $ testCases


{- | Test a generator. -}
sample :: Arbitrary a => IO [a]
sample = replicateM 10 arbitrary



-- Instances --

instance Arbitrary Int
  where
  arbitrary = randomIO

instance Arbitrary Char
  where
  arbitrary = fmap chr (randomRIO (0, ord (maxBound :: Char)))

instance
  (Arbitrary a) =>
  Testable (a -> Bool)
  where
  test assertion = fmap assertion arbitrary

instance
  (Arbitrary a, Arbitrary b) =>
  Testable (a -> b -> Bool)
  where
  test assertion = assertion <$> arbitrary <*> arbitrary

instance
  (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Testable (a -> b -> c -> Bool)
  where
  test assertion =
        assertion
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary



-- Interface --

class Arbitrary a
  where
  arbitrary :: IO a

class Testable f
  where
  test :: f -> IO Bool
