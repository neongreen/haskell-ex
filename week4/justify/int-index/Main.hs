{-# LANGUAGE LambdaCase, TypeApplications, OverloadedStrings, BangPatterns #-}

module Main where

import Data.Text as Text
import Data.Text.IO as Text
import Text.Read (readMaybe)
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.Semigroup
import Data.Maybe
import Data.Bool
import Data.Ratio
import System.Exit

data Line = Line
  { lineWidth :: Int
  , lineWords :: NonEmpty Text }

initLine :: Text -> Line
initLine w = Line (Text.length w) (w :| [])

appendLine :: Line -> Line -> Line
appendLine line1 line2 =
  Line
    (lineWidth line1 + 1 + lineWidth line2)
    (lineWords line1 <>    lineWords line2)

nonEmptyTails :: NonEmpty a -> NonEmpty [a]
nonEmptyTails = NonEmpty.fromList . List.tail . List.tails . NonEmpty.toList

justify :: Int -> Text -> Text
justify desiredLineWidth =
  Text.unlines . mergeLines . groupWords . List.map initLine . Text.words
  where
    groupWords :: [Line] -> [Line]
    groupWords = List.unfoldr (fmap @Maybe groupWords' . nonEmpty)

    groupWords' :: NonEmpty Line -> (Line, [Line])
    groupWords' lines =
      fromMaybe (NonEmpty.head groupings) $
        NonEmpty.last <$> nonEmpty goodGroupings
      where
        goodGroupings = NonEmpty.takeWhile (fits . fst) groupings
        groupings =
          NonEmpty.zip
            (NonEmpty.scanl1 appendLine lines)
            (nonEmptyTails lines)

    fits :: Line -> Bool
    fits line = lineWidth line <= desiredLineWidth

    mergeLines :: [Line] -> [Text]
    mergeLines []     = []
    mergeLines [line] = [(Text.unwords . NonEmpty.toList . lineWords) line]
    mergeLines (line : lines) = mergeLine line : mergeLines lines

    mergeLine :: Line -> Text
    mergeLine line =
      concatSpaces
        (NonEmpty.toList (lineWords line))
        (distributeExcessSpaces
          (NonEmpty.length (lineWords line) - 1)
          (desiredLineWidth - lineWidth line))

concatSpaces :: [Text] -> [Int] -> Text
concatSpaces ts     []     = Text.unwords ts
concatSpaces (t:ts) (s:ss) =
  t <> Text.replicate (s + 1) " " <> concatSpaces ts ss

distributeExcessSpaces
  :: Int   -- position count
  -> Int   -- excess space count
  -> [Int] -- space by positions
distributeExcessSpaces positions n = List.take positions (distrib freq)
  where
    freq = n % positions
    distrib !prob =
      let k = floor prob :: Int
      in k : distrib (prob + freq - fromIntegral k)

main :: IO ()
main = do
  width <- do
    s <- Text.getLine
    case readMaybe @Int (Text.unpack s) of
      Just n | n >= 0 -> return n
      _               -> do
        Text.putStrLn "Not a valid width."
        exitFailure
  line <- Text.getLine
  Text.putStrLn (justify width line)
