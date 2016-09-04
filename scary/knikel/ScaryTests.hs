-- run with: stack runghc -- -Wall ScaryTests.hs
import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import Scary (scaryWords, isScary)
import System.Exit (ExitCode(..), exitWith)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList isScaryTests
       , TestList scaryTests
       ]

isScaryTests :: [Test]
isScaryTests =
  [ testCase "isScary: False on empty string" $
    False @=? isScary ""
  , testCase "isScary can recognize a *scary* word" $
    True @=? isScary "baaed"
  , testCase "isScary can recognize a *non-scary* word" $
    False @=? isScary "notscary"
  , testCase "isScary can handle non-lowercase word" $
    False @=? isScary "Iraq"
  , testCase "isScary can handle non-alphabet word" $
    False @=? isScary "zip's"
  ]

scaryTests :: [Test]
scaryTests =
  [ testCase "No scary words in the empty list" $
    [] @=? scaryWords []
  , testCase "No scary words in the list" $
    [] @=? scaryWords ["no", "no", "no"]
  , testCase "Two scary words in the list" $
    [ "baaed", "baaed" ] @=? scaryWords ["no", "baaed", "baaed"]
  ]
