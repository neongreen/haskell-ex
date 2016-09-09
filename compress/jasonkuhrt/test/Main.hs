
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
      let g = QC.oneof [QC.vector 1, QC.vector 2] in
        QC.forAll g $ \string ->
          let l = last (Compress.put ("Hello world.Hello world." ++ string))
          in  shouldBe l (Left string)

    it "given string divisable by 3 does not end in empty string" $
      let string = "123456789"
      in  shouldBe (Compress.put string) [Left string]
