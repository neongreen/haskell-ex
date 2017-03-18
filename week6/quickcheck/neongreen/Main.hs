{-# LANGUAGE
FlexibleContexts,
TypeFamilies,
DefaultSignatures,
TupleSections,
NoImplicitPrelude
  #-}


module Main where


import BasePrelude
-- Randomness
import System.Random


main = do
  putStrLn "=== should pass ==="
  check propAeqA
  putStrLn ""
  putStrLn "=== should fail ==="
  check propAeqB
  check propAlwaysSorted
  check propNeverSorted
  check propAlwaysLong

----------------------------------------------------------------------------
-- Examples (to be run with 'check')
----------------------------------------------------------------------------

-- | This is a very simple property – A is equal to itself. It should pass.
propAeqA :: String -> Bool
propAeqA a = a == a

-- | This is another simple property – any string is equal to any other
-- string. It should fail.
propAeqB :: String -> String -> Bool
propAeqB a b = a == b

-- | “All lists are sorted”. Another one that should fail.
propAlwaysSorted :: [Int] -> Bool
propAlwaysSorted xs = sort xs == xs

-- | “No non-empty lists are sorted”. This one should fail too, but it
-- depends on the test runner not being entirely stupid about generating
-- cases.
propNeverSorted :: [Int] -> Bool
propNeverSorted xs = null xs || sort xs /= xs

-- | “All lists are longer than 5 elements long”. Another sanity check that
-- should fail.
propAlwaysLong :: [Int] -> Bool
propAlwaysLong xs = length xs >= 5

----------------------------------------------------------------------------
-- Test runner
----------------------------------------------------------------------------

check :: (Testable a, ShowTest (TestCase a)) => a -> IO ()
check f = runTests (0 :: Int)
  where
    runTests 100 = putStrLn "OK, 100 tests passed."
    runTests i = do
      testCase <- arbitrary (i * 10)
      if test f testCase
        then runTests (i+1)
        else runShrinking (i+1) (0 :: Int) testCase

    runShrinking n c testCase = do
      let shrunk = find (not . test f) (shrink testCase)
      case shrunk of
        Nothing -> printf "FAIL (test #%d, %d shrinks): %s\n"
                          n c (unwords (showTest testCase))
        Just x  -> runShrinking n (c+1) x

----------------------------------------------------------------------------
-- 'Testable' class for testing functions
----------------------------------------------------------------------------

class Arbitrary (TestCase a) => Testable a where
  type TestCase a
  test :: a -> TestCase a -> Bool

instance Testable Bool where
  type TestCase Bool = ()
  test f () = f

instance (Arbitrary x, Testable a) => Testable (x -> a) where
  type TestCase (x -> a) = (x, TestCase a)
  test f (x, rest) = test (f x) rest

----------------------------------------------------------------------------
-- 'ShowTest' class for showing test cases (“lists” made out of tuples)
----------------------------------------------------------------------------

class ShowTest a where
  showTest :: a -> [String]

instance ShowTest () where
  showTest () = []

instance (Show a, ShowTest b) => ShowTest (a, b) where
  showTest (a, b) = show a : showTest b

----------------------------------------------------------------------------
-- 'Arbitrary' class for generating random values
----------------------------------------------------------------------------

class Arbitrary a where
  -- | Generate a random value, constrained by “size” (which can be
  -- interpreted, well, arbitarily).
  arbitrary :: Int -> IO a
  default arbitrary :: Random a => Int -> IO a
  arbitrary _ = randomIO

  -- | Produce /strictly smaller/ testcases out of a testcase. If there are
  -- none, return an empty list.
  shrink :: a -> [a]
  shrink = const []

instance Arbitrary () where
  arbitrary _ = return ()

instance Arbitrary Bool

instance Arbitrary Char where
  arbitrary _ = do
    ascii <- randomIO :: IO Bool
    if ascii then randomRIO ('\0','\255') else randomIO

  shrink c
    | c `elem` simple = []
    | otherwise =
        simple ++
        concat ["\n\t\r" | isControl     c && c `notElem` "\n\r\t"] ++
        concat [",.-:()" | isPunctuation c && c `notElem` ",.-:()"]
    where
      simple = "abcABC0123"

instance Arbitrary Int where
  arbitrary n = randomRIO (-n, n)

  shrink n = delete n $ nub $ concat [
    [n `div` 5, n `div` 3, n `div` 2],
    [abs n | n < 0],
    [n-1 | n > 0],
    [n+1 | n < 0] ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary n = (,) <$> arbitrary n <*> arbitrary n

  shrink (a, b) = concat [
    (,) <$> shrink a <*> shrink b,
    (,b) <$> shrink a,
    (a,) <$> shrink b ]

instance Arbitrary a => Arbitrary [a] where
  arbitrary n = (`replicateM` arbitrary n) =<< randomRIO (0, n)

  {- The implementation of 'shrink' for lists could be better:

  >>> shrink "XyZ"
  ["","X","Z","Xy","yZ","yZ","Xy","aaa"]

  It'll never produce “abc”, for instance, even if it'd be nice if it
  would. It also never shrinks -only some- elements, and never throws out
  elements from the middle of the list.
  -}
  shrink []  = []
  shrink [x] = [] : map (:[]) (shrink x)
  shrink xs  = [] :
               -- try to decrease length
               concat [[take n xs, drop (len - n) xs]
                        | n <- takeWhile (< len) (map (2^) [0..])] ++
               -- try to drop only one element
               [tail xs, init xs] ++
               -- try to shrink elements (we have to shrink at least one)
               [map fst shrunk | any snd shrunk]               
    where
      len = length xs
      -- for each element we provide a possibly-shrunk version, but also
      -- record whether we actually managed to shrink anything or had to
      -- reuse the original element
      shrunk = [(head (s ++ [x]), not (null s)) | x <- xs, let s = shrink x]

