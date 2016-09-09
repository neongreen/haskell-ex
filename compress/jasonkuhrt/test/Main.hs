
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



  describe "Compress.put" $ do

    it "given empty string returns []" $ do
      shouldBe (Compress.put "") []

    it "given string ending in non-matching 1-2 Chars ends in Left" $ do
      let g = QC.resize 2 (QC.listOf1 (QC.arbitrary :: QC.Gen Char)) in
        QC.forAll g $ \string ->
          let l = last (Compress.put ("Hello world.Hello world." ++ string))
          in  shouldBe l (Left string)
