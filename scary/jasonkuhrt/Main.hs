-- # Scary

-- Given a letter-to-number mapping, find all "scary" words in the dictionary.
-- The mapping system is A=1, B=2, ..., Z=26.
-- A "scary" word is any word whose sum is 13.
-- The dictionary can be found at /usr/share/dict/words (macOS)

import qualified Data.Char as Char



main :: IO ()
main = do
  word <- getLine
  print . isScary $ word



letterValue :: Char -> Integer
letterValue =
  doLetterValue .
  toInteger .
  Char.ord .
  Char.toLower
  where
  doLetterValue :: Integer -> Integer
  doLetterValue charOrd
    | isInRange charOrd = charOrd - offset
    | otherwise = 0
    where
    offset = 96
    isInRange = isInRangeOf (97, 122)



isScary :: String -> Bool
isScary =
  (== scaryNumber) .
  sum .
  fmap letterValue
  where
  scaryNumber = 13



isInRangeOf :: (Integer, Integer) -> Integer -> Bool
isInRangeOf (rangeMin, rangeMax) n =
  n >= rangeMin && n <= rangeMax
