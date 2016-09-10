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

* Add Chunk Compressed + Advance Track

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

* Push Chunk Compressed + Advance Track

COMPR foo(0,3)x(3,4)
TRACK foofooxfoox|

* When cursor index is at end DONE!

-}

-- TODO function docs

module Compress where



type Compressed = [CompressedChunk]
type CompressedChunk = Either String CompressionRef
type CompressionRef = (Int, Int)
type Track = (String, String)



-- Undoing Compression --

remove :: Compressed -> String
remove = foldl go "" where

  go :: String -> CompressedChunk -> String
  go uncompressed (Right ref)   = deRef uncompressed ref
  go uncompressed (Left string) = uncompressed ++ string

  deRef :: String -> CompressionRef -> String
  deRef uncompressed (i, size) = uncompressed ++ takeSlice i size uncompressed



-- Doing Compression --

put :: String -> Compressed
put string = go (makeTrack string) where

  go :: Track -> Compressed
  go track@(_,ahead)
    | null ahead       = []
    | length ahead < 3 = [Left ahead]
    | otherwise        =
      case tryCompressingChunk track of
      Nothing ->
        let size = measureUncompressableChunk track
        in  Left (take size ahead) : go (advanceCursor size track)
      Just ref ->
        let (i, size) = ref
        in Right ref : go (advanceCursor size track)



makeTrack = (,) ""

advanceCursor :: Int -> Track -> Track
advanceCursor n (behind, ahead) =
  (behind ++ take n ahead, drop n ahead)


-- NOTE Track MUST have 3+ ahead
-- NOTE that "3" is the minimum compressable size
measureUncompressableChunk :: Track -> Int
measureUncompressableChunk = go 0 where
  go size (_, [])               = size
  go size track@(behind, ahead) =
    case findMatchStart behind (take 3 ahead) of
      Nothing -> go (size + 1) (advanceCursor 1 track)
      Just _  -> size



-- NOTE Track MUST have 3+ ahead
-- NOTE that "3" is the minimum compressable size
tryCompressingChunk :: Track -> Maybe CompressionRef
tryCompressingChunk (behind, ahead) =
  go Nothing (take 3 ahead) where

  go :: Maybe CompressionRef -> String -> Maybe CompressionRef
  go lastMatch prospect =
    case findMatchStart behind prospect of
    Nothing ->
      lastMatch
    Just i  ->
      let prospectSize = length prospect
          newMatch = Just (i, prospectSize)
      in if elem prospectSize [length behind, length ahead]
         then newMatch
         else go newMatch (take (prospectSize + 1) ahead)







-- General Helpers --



findMatchStart :: Eq a => [a] -> [a] -> Maybe Int
findMatchStart list subSeq = go 0 list where
  go i xs
    | takeChunk xs == subSeq = Just i
    | length xs < size       = Nothing
    | otherwise              = go (i + 1) (tail xs)
  size = length subSeq
  takeChunk = take (length subSeq)



takeWhileEqual :: Eq a => [a] -> [a] -> [a]
takeWhileEqual xs zs =
  fmap fst . takeWhile (uncurry (==)) $ zip xs zs



takeSlice :: Int -> Int -> [a] -> [a]
takeSlice i size = take size . drop i
