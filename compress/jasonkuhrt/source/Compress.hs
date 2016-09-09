-- compress --

-- Given a string, replace repeating substrings (of length >= 3) with a lookup
-- to the first instance. Performance is not a goal.



module Compress where



type Compressed = [CompressedChunk]
type CompressedChunk = Either String CompressionRef
type CompressionRef = (Int, Int)



-- Undoing Compression --

remove :: Compressed -> String
remove = foldl go "" where

  go :: String -> CompressedChunk -> String
  go uncompressed (Right ref)   = deRef uncompressed ref
  go uncompressed (Left string) = uncompressed ++ string


  deRef :: String -> CompressionRef -> String
  deRef uncompressed (i,len) = uncompressed ++ takeFrom i len uncompressed



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
      let delta = advanceWhileMatch mmsi track
      in  Right (mmsi, delta) : go (-1) (progress + delta)
    | otherwise          =
      let (matchStart, delta) = advanceWhileMatchNot track
      in  Left (take delta trackAhead) : go matchStart (progress + delta)
    where
    track = splitAt progress string
    (_, trackAhead) = track



advanceWhileMatchNot :: (String, String) -> (Int, Int)
advanceWhileMatchNot = go 0 where

  go delta (_, [])               = (-1, delta)
  go delta track@(behind, ahead) =
    case findMatchStart behind (take 3 ahead) of
      Nothing         -> go (delta + 1) (trackAdvance track)
      Just matchStart -> (matchStart, delta)

  trackAdvance (behind, a:as) = (behind ++ [a], as)







advanceWhileMatch :: Int -> (String, String) -> Int
advanceWhileMatch matchStart (behind, ahead) =
  length $ takeWhileEqual (drop matchStart behind) ahead



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



takeFrom :: Int -> Int -> [a] -> [a]
takeFrom i len = take len . snd . splitAt i



slice :: Int -> Int -> [a] -> [a]
slice i size = take size . drop i
