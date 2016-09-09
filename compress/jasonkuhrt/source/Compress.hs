-- compress --

-- Given a string, replace repeating substrings (of length >= 3) with a lookup
-- to the first instance. Performance is not a goal.

-- string
--
-- (Just i) progress
  -- delta = advanceWhileMatch i behind ahead
  -- Right (i, delta) : go Nothing (progress + delta)
-- Nothing progress
  -- (matchStart, delta) = advanceWhileMatchNot behind ahead
  -- Left (take delta ahead) : go (Just matchIndex) (progress + delta)

-- FIND A MATCH START
-- blahfoobar |blahfoogoo
-- ^^^         ^^^
--
-- FIND A MATCH END
-- blahfoobar blah|foogoo
--     ^           ^
-- blahfoobar blahf|oogoo
--      ^           ^
-- blahfoobar blahfo|ogoo
--       ^           ^
-- blahfoobar blahfoo|goo
--        ^           ^
--
-- RESTART. Reset The
-- blahfoobar blahfoo|goo
--        ^^^         xxx



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
      Just matchStart ->
        let delta = advanceWhileMatch matchStart behind ahead
        in  Right (matchStart, delta) : compressDo (progress + delta)
      Nothing ->
        let (matchStart, len) = advanceWhileMatchNot behind ahead
        in  Left (take len ahead) : compressDo (progress + len)
    where
    (behind, ahead) = splitAt progress string



advanceWhileMatchNot :: String -> String -> (Int, Int)
advanceWhileMatchNot = go 0 where

  go delta _ []         = (-1, delta)
  go delta behind ahead =
    case findMatchStart behind (take 3 ahead) of
    Nothing ->
      go (delta + 1) (behind ++ take 1 ahead) (drop 1 ahead)
    Just matchStart -> (matchStart, delta)



findMatchStart :: Eq a => [a] -> [a] -> Maybe Int
findMatchStart = findMatchStartDo 0 where
  findMatchStartDo i xs vs
    | take (length vs) xs == vs = Just i
    | length xs < length vs     = Nothing
    | otherwise                 = findMatchStartDo (i + 1) (tail xs) vs



advanceWhileMatch :: Int -> String -> String -> Int
advanceWhileMatch matchStart behind ahead =
  length $ takeWhileEqual (drop matchStart behind) ahead




-- General Helpers --

takeWhileEqual :: Eq a => [a] -> [a] -> [a]
takeWhileEqual xs zs =
  fmap fst . takeWhile (uncurry (==)) $ zip xs zs

takeFrom :: Int -> Int -> [a] -> [a]
takeFrom i len = take len . snd . splitAt i

slice :: Int -> Int -> [a] -> [a]
slice i size = take size . drop i
