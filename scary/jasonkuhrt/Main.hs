-- # Scary

-- Given a letter-to-number mapping, find all "scary" words in the dictionary.
-- The mapping system is A=1, B=2, ..., Z=26.
-- A "scary" word is any word whose sum is 13.
-- The dictionary can be found at /usr/share/dict/words (macOS)

import qualified Data.Char as Char
-- import Control.Monad



main :: IO ()
main = printScaryDict



printScaryDict :: IO ()
printScaryDict =
  print
  . filter isScary
  -- takeWhile optimization: Words in this dictionary are ordered alphabetically. Therefore we can stop searching the dictionary once words are no longer possibly scary (all words after "n", see "isPossiblyScary" for more details).
  . takeWhile isPossiblyScary
  . words
  =<< readFile "/usr/share/dict/words"



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
isScary = (== scaryNumber) . sum . fmap letterValue



-- Useful for short circuit optimizations: If the word's first character _number_ is beyond the scary number then naturally the word's sum *must* be greater than (importantly to us: _not_) the scary number.

-- In this program "13" is the scary number which results in any word starting with the letter "m" onward never being scary.

isPossiblyScary :: String -> Bool
isPossiblyScary "" = False
isPossiblyScary word = (<= scaryNumber) . letterValue . head $ word



scaryNumber :: Integer
scaryNumber = 13



isInRangeOf :: (Integer, Integer) -> Integer -> Bool
isInRangeOf (rangeMin, rangeMax) n =
  n >= rangeMin && n <= rangeMax
