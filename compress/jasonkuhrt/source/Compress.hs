-- compress --

-- Given a string, replace repeating substrings (of length >= 3) with a lookup
-- to the first instance. Performance is not a goal.


{- ALGORITHM SKETCH/EXAMPLE

* Terminology

"|" is a cursor, used to track progress on "track" which is the
combination of what is behind and ahead of cursor, e.g.:

behind|ahead

Example: Given "foofooxfoox"

* If string less than six characters we cannot compress.
* Advance cursor to after min compressable size
* Add Chunk Raw + Advance Track

COMPR foo
TRACK foo|fooxfoox

* Setup Check (size starts at min compressable size)

COMPR foo
TRACK foo|fooxfoox
          ^^^
* Match Scan
  * Pick the match that consumes the most off AHEAD
  * If no match
    1 Recursively advance scan until end of BEHIND
  * If match
    Recursively exapand the check size until the last match must be reverted to, in the following cases:
      2 No longer match (END CASE 1)
      3 Check Size exceeds AHEAD size
      4 Check Size exceeds BEHIND size

COMPR foo
TRACK foo|fooxfoox  foo|fooxfoox  foo|fooxfoox
      ^^^ ^^^       ^^^ ^^^^      ^^^ ^^^
      MATCH         END CASE 4    REVERT

* Add Chunk CompressedString + Advance Track

COMPR foo(0,3)
TRACK foofoo|xfoox

* Setup Check + Match Scan

COMPR foo(0,3)
TRACK foofoo|xfoox  foofoo|xfoox  foofoo|xfoox  foofoo|xfoox
      ^^^    ^^^     ^^^   ^^^      ^^^  ^^^       ^^^ ^^^
                                                   END CASE 1
* Push Chunk Raw + Advance Track

COMPR foo(0,3)x
TRACK foofoox|foox

* Setup Check + Match Scan

COMPR foo(0,3)
TRACK foofoox|foox  foofoox|foox  foofoox|foox  foofoox|foox  foofoox|foox
      ^^^     ^^^   ^^^^    ^^^^   ^^^^   ^^^^    ^^^^  ^^^^     ^^^^ ^^^^
      MATCH                                                      MATCH

      foofoox|foox  foofoox|foox
      ^^^^^   ^^^^^    ^^^^ ^^^^
      END CASE 3    REVERT

* Push Chunk CompressedString + Advance Track

COMPR foo(0,3)x(3,4)
TRACK foofooxfoox|

* When cursor index is at end DONE!

-}

-- TODO function docs

module Compress where

import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Either as Either



type CompressedString = [Token]
type Token = Either String Reference
type Reference = (Int, Int)
type Track = (String, String)



-- Decompression --

-- [1] Tying the knot, see wiki.haskell.org/Tying_the_Knot
--

remove :: CompressedString -> String
remove compressed = decompressed where

  decompressed = concatMap deRef compressed -- [1]

  deRef :: Token -> String
  deRef (Left string)     = string
  deRef (Right (i, size)) = takeSlice i size decompressed -- [1]



-- Compression --
-- TODO Refactor. See https://github.com/neongreen/haskell-ex/blob/master/compress/neongreen/Main.hs#L55-L72

put :: String -> CompressedString
put string = compact (go "" string) where

  go :: String -> String -> CompressedString
  go _ []       = []
  go p ahead
    | size < 3  =
      let (taken, rest) = splitAt 1 ahead in
      Left taken : go (p ++ taken) rest
    | otherwise =
      let (taken, rest) = splitAt size ahead in
      Right ref : go (p ++ taken) rest
    where
    (i, size) = ref
    ref       = longestMatch p ahead



compact :: CompressedString -> CompressedString
compact = go where
  go [] = []
  go (Right ref:xs)   = Right ref : go xs
  go (Left string:xs) = Left (string ++ concat (Either.lefts more)) : go rest
    where
    (more, rest) = span Either.isLeft xs



{- Find the longest match of B in A

For example, given "bacba" "bac"

        1. |bac|ba    2. bac|ba|
           |bac|            |ba|c

    Ref    (0,3)         (4,2)

pick Match 1 since 3 > 2.

* A match is represented by a tuple: First, the index
of where the match starts; Second, the match length.

* A match length of `0` means there were no matches,
for example `(_,0)`. -}
longestMatch :: String -> String -> Reference
longestMatch v x =
  -- Get a list of all possible matches.
  -- Then just select the one of maximum size.
  List.maximumBy (Ord.comparing snd) matches
  where
  matches = [(i, length (takeWhileEqual rest x))
            | (i, rest) <- zip [0..] (List.tails v)]



-- General Helpers --



takeWhileEqual :: Eq a => [a] -> [a] -> [a]
takeWhileEqual xs zs =
  fmap fst . takeWhile (uncurry (==)) $ zip xs zs



takeSlice :: Int -> Int -> [a] -> [a]
takeSlice i size = take size . drop i
