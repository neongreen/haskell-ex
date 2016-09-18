{-# LANGUAGE UnicodeSyntax #-}

module Main
  where

import           Data.HashMap.Strict as H (HashMap, fromList, lookupDefault)
import qualified Data.Text           as T

-- To assign the value of each letter, I did a zip between [1,2,3,..]
-- and the other letters. Later, I just ask for this map in O (1)
-- the value. With the lookupDefault 0, I don't worry about if actually
-- it is not a letter.
charset ∷ H.HashMap Char Int
charset = H.fromList $ zip ['A'..'Z'] [1..]

sumWord ∷ T.Text → Int
sumWord = sum . map (\c → lookupDefault 0 c charset) . T.unpack . T.toUpper
isScary ∷ T.Text → Bool
isScary = (13 ==) . sumWord

-- I just print the word in case it is actually a scary word.
main ∷ IO ()
main = putStr
  . T.unpack
  . T.unlines
  . filter isScary
  . T.words
  . T.pack
  =<< getContents
