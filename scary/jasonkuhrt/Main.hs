-- # Scary

-- Given a letter-to-number mapping, find all "scary" words in the dictionary.
-- The mapping system is A=1, B=2, ..., Z=26.
-- A "scary" word is any word whose sum is 13.
-- The dictionary file can be found at /usr/share/dict/words (macOS)



import qualified Data.Char as Char
import qualified Data.Ix as Range



main :: IO ()
main = printScaryDictWords "/usr/share/dict/words"



printScaryDictWords :: String -> IO ()
printScaryDictWords dictionaryFilePath =
  -- TODO Handle the possible exception thrown by readfile!
  -- Refer to suggestion from neongreen:
  -- https://haskell-learning.slack.com/archives/alpha/p1472742690000172
  print
  . filter isScary
  -- Optimization: This function expects a list of words sorted alphabetically
  -- so that we can stop searching once words are no longer possibly scary
  -- (all words after "n", see "isPossiblyScary" for more details).
  . takeWhile isPossiblyScary
  . words
  =<< readFile dictionaryFilePath



isScary :: String -> Bool
isScary = (== scaryNumber) . sum . fmap letterValue



-- Useful for short circuit optimizations: If the word's first character
-- _number_ is beyond the scary number then naturally the word's sum *must*
-- be greater than (importantly to us: _not_) the scary number.

-- In this program "13" is the scary number which results in any word starting
-- with the letter "m" onward never being scary.

isPossiblyScary :: String -> Bool
isPossiblyScary "" = False
isPossiblyScary word = (<= scaryNumber) . letterValue . head $ word



scaryNumber :: Int
scaryNumber = 13



letterValue :: Char -> Int
letterValue char
  | isEligible charNormalized = calcValue charNormalized
  | otherwise = 0
  where
  isEligible = Range.inRange ('a','z')
  charNormalized = Char.toLower char
  -- The mapping system is simply A=1, B=2, ... Z=26. We can take advantage of
  -- unicode numbers+ordering to provide this mapping because unicode a-z
  -- characters are contiguously numbered (97-122)! We just need to subtract
  -- by value of our system's first unicode point (minus 1, because we
  -- are one-based rather than zero-based) to align our numbering.
  calcValue = subtract (Char.ord 'a' - 1) . Char.ord

  -- Alternative implementation:
  -- This solution reads better but proved slower by multiple seconds.
  --
  -- | isAll [Char.isAscii, Char.isLetter] char = calcValue char
  -- | otherwise = 0
  -- where
  -- calcValue = subtract (Char.ord 'a' - 1) . Char.ord . Char.toLower
