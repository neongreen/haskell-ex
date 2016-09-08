-- compress --

-- Given a string, replace repeating substrings (of length >= 3) with a lookup
-- to the first instance. Performance is not a goal.



module Main where

main :: IO ()
main = undefined

type Compressed = [Either String (Int,Int)]



compress :: String -> Compressed
compress xs
  | length xs < 6 = [Left xs]
  | otherwise     = compressDo "" xs


-- TODO
-- "hello hello man is this thing working or is this thing working?"
-- "hello man is thing workor?"
-- SHOULD BE:
-- "hello man is this thing working or?"

compressDo :: String -> String -> Compressed
compressDo behind ahead
  | length ahead < 3 = [Left ahead]
  | otherwise        =
    case findMatchStart behind (take 3 ahead) of
    Nothing ->
      Left (take 1 ahead) :
      compressDo (behind ++ take 1 ahead) (drop 1 ahead)
    Just i ->
      let len = findMatchLength (snd . splitAt i $ behind) ahead
          behindNow = behind ++ take len ahead
          aheadNow = drop len ahead
      in Right (i,len) : compressDo behindNow aheadNow



uncompress :: Compressed -> String
uncompress = foldl go "" where
  go uncompressed (Right ref)   = deRef uncompressed ref
  go uncompressed (Left string) = uncompressed ++ string


  deRef :: [a] -> (Int, Int) -> [a]
  deRef uncompressed ref = uncompressed ++ takePart ref uncompressed

  takePart :: (Int, Int) -> [a] -> [a]
  takePart (i, len) = take len . snd . splitAt i



findMatchStart :: Eq a => [a] -> [a] -> Maybe Int
findMatchStart = findMatchStartDo 0 where
  findMatchStartDo i xs vs
    | take (length vs) xs == vs = Just i
    | length xs < length vs     = Nothing
    | otherwise                 = findMatchStartDo (i + 1) (tail xs) vs



findMatchLength :: Eq a => [a] -> [a] -> Int
findMatchLength matchBehind = -- eta reduce
  length . takeWhile (uncurry (==)) . zip matchBehind
