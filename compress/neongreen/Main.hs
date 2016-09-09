import Data.Ord
import Data.List
import Data.Either
import Test.QuickCheck

main = do
  quickCheck $ \xs -> decompress (compress xs) === xs

type Token = Either String (Int, Int)

{- |
Return /all/ matches of a string against another string, even the ones with length 0.

@matches i p s@ will find all matches of @s@ or any its prefix against @p@:

>>> matches 0 "abacaba" "bac"
[(0,0),
 (1,3),  -- offset 1: “a|bacaba” matches all 3 characters of “bac”
 (2,0),
 (3,0),
 (4,0),
 (5,2),  -- offset 5: “abaca|ba” matches first 2 characters of “bac”
 (6,0)]
-}
matches
  :: Int            -- ^ Offset of the string
  -> String         -- ^ Huge string we're matching against
  -> String         -- ^ String we're trying to match
  -> [(Int, Int)]
matches _ [] s = []
matches i p s =
  let matchLength = length (takeWhile id (zipWith (==) p s))
  in  (i, matchLength) : matches (i+1) (tail p) s

{- |
Find longest match among all matches returned by 'matches'.
-}
longestMatch :: String -> String -> (Int, Int)
longestMatch p s = case matches 0 p s of
  [] -> (0,0)
  xs -> maximumBy (comparing snd) xs

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
          (match, rest) = splitAt n (c:s)
      in if n < 3 then Left [c]    : go (p ++ [c]) s
                  else Right (i,n) : go (p ++ match) rest

decompress :: [Token] -> String
decompress s = go [] s
  where
    go p [] = p
    go p (Left x      : xs) = go (p ++ x) xs
    go p (Right (i,n) : xs) = go (p ++ take n (drop i p)) xs
