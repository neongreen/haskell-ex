module Main where

import           Data.List
import           Data.Vector     (Vector)
import qualified Data.Vector     as V
import           Test.QuickCheck

-- | Start position, Length
type Interval = (Int, Int)
-- | Similar to above: Start position, Piece of sequence.
-- | We need to keep track of position within original sequence.
type Sub a = (Int, Vector a)
type Compressed a = Either (Sub a) Interval

vCompressWith :: Eq a => Sub a -> Sub a -> [Compressed a]
vCompressWith (sp, s) (vp, v)
  | ls == 0 || lv == 0 || ls > lv = [Left (vp, v)]
  | otherwise = replace 0 repeats
  where
    ls = V.length s
    lv = V.length v
    -- | Indices where first letter of subseq is found in seq -
    -- | just a small kick of performance.
    is = V.toList $ V.elemIndices (V.head s) (V.slice 0 (lv-ls+1) v)
    -- | Among those above, pick the ones that are the start
    -- | of actual subseq in the seq; drop the first occurance.
    -- | Note: we need to keep track of current pos in the seq (vpos),
    -- | to avoid overlapping subseqs, e.g. "aa" in "aaa".
    repeats = reverse $ fst $ foldl' f ([], 0) is
    f (finds, vpos) spos
      | spos < vpos || spos + ls > lv = (finds, vpos)
      | vp + spos < sp && vp + spos + ls > sp = (finds, spos)
      | sp == vp + spos = (finds, spos + ls)
      | otherwise = if s == V.slice spos ls v
          then (spos:finds, spos + ls)
          else (finds, spos)
    -- | Replace the repeating subseqs with Right (start, length),
    -- | The leftovers will be turned into Left Sub a.
    replace pos (next:rest) =
      let
          prefix = V.slice pos (next - pos) v
          replacement = Right (sp, ls) : replace (next + ls) rest
      in
        if V.null prefix
        then replacement
        else Left (vp+pos, prefix) : replacement
    replace pos []
      | pos < lv = [Left (vp+pos, V.slice pos (lv-pos) v)]
      | otherwise = []

-- | Same idea as vCompressWith, but here we look for subseq among
-- | the Left in [Compressed a]
compressWith :: (Eq a) => Sub a -> [Compressed a] -> [Compressed a]
compressWith (sp, s) = concatMap f
  where
    f (Right sub)= [Right sub]
    f (Left (vp, v)) = cv where
      cv = vCompressWith (sp, s) (vp, v)

slice :: Int -> Int -> [Compressed a] -> Vector a
slice p n c = case c of
  [] -> V.empty
  (Right _ : cs) -> slice p n cs
  (Left (pos, v) : cs) ->
    if p >= pos && p+n <= pos + V.length v
    then V.slice (p-pos) n v
    else slice p n cs


minSubLength = 3

compress :: String -> [Either String (Int, Int)]
compress str = result where
  v = V.fromList str
  lv = V.length v

  result = map remap $ loop (div lv 2) 0 [Left (0, v)]

  remap (Left (_, sub)) = Left $ V.toList sub
  remap (Right smth) = Right smth

  loop n p cmpd
    | n < minSubLength = cmpd
    | p > lv - n = loop (n-1) 0 cmpd
    | otherwise = loop n (p+1) further
    where
      sub = slice p n cmpd
      further = if V.null sub then cmpd else compressWith (p, sub) cmpd


decompress :: [Either String (Int, Int)] -> String
decompress c = V.toList $ foldl' f V.empty c where
  f acc (Left s) = acc V.++ V.fromList s
  f acc (Right (i, n)) = acc V.++ V.slice i n acc


prop_compdecomp :: String -> Bool
prop_compdecomp s = s == (decompress . compress) s

main :: IO ()
main = quickCheck prop_compdecomp
