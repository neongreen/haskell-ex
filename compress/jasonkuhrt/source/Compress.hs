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
put string = compressDo 0 where

  compressDo :: Int -> Compressed
  compressDo progress
    | null ahead       = []
    | length ahead < 3 = [Left ahead]
    | otherwise        =
      case findMatchStart behind (take 3 ahead) of
      Nothing ->
        let len = findLengthMatchless behind ahead
        in  Left (take len ahead) : compressDo (progress + len)
      Just i ->
        let len = findLengthMatch (snd . splitAt i $ behind) ahead
        in  Right (i, len) : compressDo (progress + len)
    where
    (behind, ahead) = splitAt progress string



findLengthMatchless :: String -> String -> Int
findLengthMatchless = findLengthMatchlessDo 0 where

  findLengthMatchlessDo i _ []         = i
  findLengthMatchlessDo i behind ahead =
    case findMatchStart behind (take 3 ahead) of
    Nothing ->
      findLengthMatchlessDo (i + 1) (behind ++ take 1 ahead) (drop 1 ahead)
    Just _ ->
      i



findMatchStart :: Eq a => [a] -> [a] -> Maybe Int
findMatchStart = findMatchStartDo 0 where
  findMatchStartDo i xs vs
    | take (length vs) xs == vs = Just i
    | length xs < length vs     = Nothing
    | otherwise                 = findMatchStartDo (i + 1) (tail xs) vs



findLengthMatch :: Eq a => [a] -> [a] -> Int
findLengthMatch matchBehind = -- eta reduce
  length . takeWhile (uncurry (==)) . zip matchBehind



-- General Helpers --

takeFrom :: Int -> Int -> [a] -> [a]
takeFrom i len = take len . snd . splitAt i
