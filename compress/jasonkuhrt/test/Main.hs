
module Main where

import Test.Hspec
import qualified Compress
import qualified Test.QuickCheck



main :: IO ()
main = hspec $ do

  describe "Test.Hspec.it" $ do

    it "must return an Expectation" $ do
      shouldBe 1 1
