import Data.Ord
import Data.List
import Data.Either
import Test.QuickCheck

main = do
  quickCheck $ \xs -> decompress (compress xs) === xs

type Token = Either String (Int, Int)  -- Starting position, length

{- |
>>> prefixLength "hello" "world"
0
>>> prefixLength "hello" "hex"
2
-}
prefixLength :: String -> String -> Int
prefixLength p s = length (takeWhile id (zipWith (==) p s))

{- |
Find longest match of two strings. For instance, @longestMatch "abacaba" "bac"@ will find two matches:

@
1. a|bac|aba   2. abaca|ba|
    |bac|              |ba|c
@

and choose the first one, because it's longer. The resulting match will be @(1,3)@ – starts from index 1, length = 3. If there's no match, length will be 0.
-}
longestMatch :: String -> String -> (Int, Int)
longestMatch p s = maximumBy (comparing snd) matches
  where
    matches :: [(Int, Int)]
    matches = [(i, prefixLength p' s) | (i, p') <- zip [0..] (tails p)]

{- |
Consolidate all characters found by 'compress' into strings.

>>> consolidate [Left "x", Left "y", Right (0,3)]
[Left "xy", Right (0,3)]
-}
consolidate :: [Token] -> [Token]
consolidate [] = []
consolidate xs =
  let (lefts, rest) = span isLeft xs
      leftsString = concat [x | Left x <- lefts]
  in  case (lefts, rest) of
        ([], Right r : rs) -> Right r : consolidate rs
        (_ , Right r : rs) -> Left leftsString : Right r : consolidate rs
        (_ , [])           -> [Left leftsString]

compress :: String -> [Token]
compress s = consolidate $ go [] s
  where
    -- The first parameter is the string we've already processed,
    -- the second – string left to process
    go :: String -> String -> [Token]
    go _ [] = []
    go p (c:s) =
      let (i,n) = longestMatch p (c:s)
          -- we can use @splitAt n@ because the match is always a prefix of
          -- the second string (i.e. @c:s@)
          (match, rest) = splitAt n (c:s)
      in if n < 3 then Left [c]    : go (p ++ [c]) s
                  else Right (i,n) : go (p ++ match) rest

decompress :: [Token] -> String
decompress xs = uncompressed
  where
    uncompressed = concatMap deRef xs
    deRef (Left x) = x
    deRef (Right (i,n)) = take n (drop i uncompressed)

{- A slower version without tying the knot:

decompress :: [Token] -> String
decompress s = go [] s
  where
    go p [] = p
    go p (Left x      : xs) = go (p ++ x) xs
    go p (Right (i,n) : xs) = go (p ++ take n (drop i p)) xs
-}
