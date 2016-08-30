{-# LANGUAGE UnicodeSyntax #-}

module Scary
  where

import           Control.Monad       (unless)
import           Data.HashMap.Strict as H (HashMap, fromList, lookupDefault)
import           Data.List           (intercalate)
import           Data.Text           as T (pack, toUpper, unpack)
import           System.IO           (isEOF)

-- To assign the value of each letter, I did a zip between [1,2,3,..]
-- and the other letters. Later, I just ask for this map in O (1)
-- the value. With the lookupDefault 0, I don't worry about if actually
-- it is not a letter.
charset ∷ H.HashMap Char Int
charset = H.fromList $ zip "ABCDEFGHIJKLMNOPRSTUVWXYZ" [1..]

sumWord ∷ String → Int
sumWord = sum . map (\c → lookupDefault 0 c charset) . unpack . T.toUpper . pack

isScary ∷ String → Bool
isScary = (13 ==) . sumWord

-- I just print the word in case it is actually a scary word.
main ∷ IO ()
main = do
  end ← isEOF
  unless end $ do
    wds ← getLine

    let scaries ∷ [String]
        scaries = filter isScary $ words wds

    unless (null scaries) $
      putStr . unlines $ scaries

    main
