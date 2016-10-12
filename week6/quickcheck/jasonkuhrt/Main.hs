{- README

-- Code Kata: Quickcheck

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

main :: IO ()
main = undefined
