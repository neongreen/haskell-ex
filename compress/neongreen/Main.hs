import Data.Ord
import Data.List
import Data.Either
import Test.QuickCheck

main = do
  let prop = \xs -> decompress (compress xs) === xs
  quickCheck prop
  quickCheck $ forAllShrink genStr shrink prop

genStr :: Gen String
genStr = concat <$> listOf (oneof [part, arbitrary])
  where
    part = growingElements ["a","bc","def","ghij","klmno","pqrstu"]

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
consolidate []             = []
consolidate (Right x : xs) = Right x : consolidate xs
consolidate xs             = Left (concat (lefts ls)) : consolidate rest
  where (ls, rest) = span isLeft xs

compress :: String -> [Token]
compress s = consolidate $ go "" s
  where
    -- The first parameter is the string we've already processed,
    -- the second – string left to process
    go :: String -> String -> [Token]
    go _ "" = []
    go p (c:cs)
      | n < 3     = Left [c]    : go (p ++ [c]) cs
      | otherwise = Right (i,n) : go (p ++ match) rest
      where
        (i,n) = longestMatch p (c:cs)
        -- we can use @splitAt n@ because the match is always a prefix of
        -- the second string (i.e. @c:s@)
        (match, rest) = splitAt n (c:cs)

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
