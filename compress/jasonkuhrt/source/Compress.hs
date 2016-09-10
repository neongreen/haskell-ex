-- compress --

-- Given a string, replace repeating substrings (of length >= 3) with a lookup
-- to the first instance. Performance is not a goal.



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
put ""     = []
put string = go (-1) 0 where

  end = length string

  go :: Int -> Int -> Compressed
  go mmsi progress -- mmsi = maybeMatchStartIndex
    | end == progress    = []
    | end - progress < 3 = [Left trackAhead]
    | mmsi >= 0          =
      case matchScan track of
      Nothing  -> [Left trackAhead]
      Just ref ->
        let (i, size) = ref
        in Right ref : go (-1) (progress + size)
    | otherwise          =
      let (matchStart, size) = advanceWhileMatchNot track
      in  Left (take size trackAhead) : go matchStart (progress + size)
    where
    track = splitAt progress string
    (_, trackAhead) = track



advanceWhileMatchNot :: Track -> (Int, Int)
advanceWhileMatchNot = go 0 where

  go size (_, [])               = (-1, size)
  go size track@(behind, ahead) =
    case findMatchStart behind (take 3 ahead) of
      Nothing         -> go (size + 1) (trackAdvance track)
      Just matchStart -> (matchStart, size)

  trackAdvance (behind, a:as) = (behind ++ [a], as)



matchScan :: Track -> Maybe CompressionRef
matchScan (behind, ahead) =
  go Nothing (take 3 ahead)
  where
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





{-
| is a cursor (use to track progress on track)
behind|ahead


Given "foofooxfoox"

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
