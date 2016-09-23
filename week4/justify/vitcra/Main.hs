module Main where

import           Data.Function (on)
import           Data.List

-- | distribute words in lines greedily
-- | note: this is not the best way
-- | best way would be to use dynamic programming
greedyLines :: Int -> String -> [[String]]
greedyLines n str = reverse $ reverse lln : lns where
  -- | The accumulator is a triple where
  -- | the first component is the return value,
  -- | the second component is the "current line" which at the end of the fold
  -- | is also the "last line". The third component is the length of the
  -- | current line.
  -- | To make it faster, (:) is used instead of (++),
  -- | but then we need to use reverse too (which in the end is still faster).
  (lns, lln, _) = foldl' f ([], [], 0) (words str)
  f (ls, cs, k) w
    | n + 1 < l = ([w] : reverse cs : ls, [], 0)
    | n + 1 < nk = (reverse cs : ls, [w], l)
    | otherwise = (ls, w : cs, nk)
    where
      l = 1 + length w
      nk = k + l

-- | put in necessary spaces to justify one line
justifyLine :: Int -> [String] -> [String]
justifyLine n ws = map addSpaces (zip ws [0..])
  where
    -- | number of words
    nw = length ws
    -- | number of spaces
    np = nw - 1
    -- | length of the words
    lws = map length ws
    -- | number of characters in the line
    nc = sum lws
    -- | number of spaces to add in for justification
    ac = n - nc
    -- | number of spaces to distribute equally,
    -- | number of spaces to fit in best way
    (ec, dc) = divMod ac np
    -- | ideal length
    ideal = fromIntegral nc / fromIntegral (dc+1)
    -- | places where to put the best to fit spaces
    ps = snd $ dP dc 0 lws

    -- | fill in the spaces
    addSpaces (w, i) = w ++ replicate nsp ' ' where
      nsp
        | i == nw - 1 = 0
        | i `elem` ps = ec+1
        | otherwise = ec

    -- | places where to add the best fitted spaces
    -- | by dynamic programming
    dP
      :: Int -- how many left to fit
      -> Int -- which initial word is the first in our subtext
      -> [Int] -- the lengths of the words in our subtext
      -> (Double, [Int]) -- (badness of our choices, choices of the places)
    dP 0 _ ls = (0, [])
    dP k iw ls = minimumBy (compare `on` fst) choices where
      m = length ls
      l = sum ls
      choices = map f [0..m-k-1]
      f i = (badness + rb, (i+iw):rs) where
        (rb, rs) = dP (k-1) (i+iw+1) $ drop (i+1) ls
        segl = fromIntegral $ sum $ take (i+1) ls -- length of the cut
        badness = (segl - ideal) ^ 2

-- | justify text
justify :: Int -> String -> String
justify n str = intercalate "\n" $ map concat jlns where
  lns = greedyLines n str
  jlns = map (justifyLine n) (init lns) ++ [map (++" ") $ last lns]

main :: IO ()
main = do
  width <- read <$> getLine
  text <- getLine
  putStrLn ""
  putStrLn $ justify width text
