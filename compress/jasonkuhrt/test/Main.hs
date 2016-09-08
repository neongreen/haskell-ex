
module Main where

import Test.Hspec
import qualified Compress
import qualified Test.QuickCheck as QC



main :: IO ()
main = hspec $ do

  describe "Compress" $ do

    it "(remove . put) is identity" $ do
      QC.property $ \string ->
        let
        f = Compress.remove . Compress.put
        in
        shouldBe (f string) string
