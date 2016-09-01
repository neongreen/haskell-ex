-- # Scary

-- Given a letter-to-number mapping, find all "scary" words in the dictionary.
-- The mapping system is A=1, B=2, ..., Z=26.
-- A "scary" word is any word whose sum is 13.
-- The dictionary can be found at /usr/share/dict/words (macOS)

import qualified Data.Char as Char
import qualified Data.List.Split as Split



main :: IO ()
main = printScaryDict



printScaryDict :: IO ()
printScaryDict = do
  -- TODO This can be optimized. First realize that any word that starts with letter "n" or later _cannot_ be scary because its sum must be higher than 13 because "n" alone is equal to 14. Second, note that words in this dictionary are ordered alphabetically. Therefore we can stop searching the dictionary after the letter "m"!
  dictionary <- readFile "/usr/share/dict/words"
  print . filter isScary . Split.splitOn "\n" $ dictionary



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
