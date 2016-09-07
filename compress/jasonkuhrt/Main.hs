-- compress --

-- Given a string, replace repeating substrings (of length >= 3) with a lookup
-- to the first instance. Performance is not a goal.



-- About Minimum Compressable String
-- Six characters, example: "foofoo"
-- This is because:
-- * We compress repeating subpatterns.
-- * minRepeat: A repeating subpattern is one that shows up 2+ times.
-- * minSubstringSize: The lookup notation we use for compression has its own
--   size which dictates the need for substrings of 3+ chars otherwise the
--   compression actually _increses_ the size.
-- * So, from this we get the simple equation:
--   minSubstringSize * minRepeat = 6

module Main where

main :: IO ()
main = undefined

-- compress :: String -> String
-- compress s
--   | length s < 6 = s -- Refer to "Minimum Compressable String"
--   | otherwise    = compressDo base pool rest
--   where
--   (base, afterBase) =  splitAt 3 s
--   (pool, rest) = splitAt 3 afterBase


-- compressDo :: String -> String -> String -> String
-- compressDo base pool (c:s) =
--   checkMatch base (pool ++ [c])


-- Recusively roll pool into base, checking for a subsequence of the dwindling pool in base during each recursion.

checkMatch :: String -> String -> String
checkMatch _ ""         = ""
checkMatch base pool@(c:s)
  | length pool < 3     = ""
  | length base < length pool =
    let (base', pool') = balanceLists base pool
    in checkMatch base' pool'
  | isSubsequence base pool = pool
  | otherwise               = checkMatch (base ++ [c]) s


-- Find zs within xs

isSubsequence :: String -> String -> Bool
isSubsequence "" _      = False
isSubsequence _ ""      = False
isSubsequence xs zs
  | xsChunk == zs       = True -- TODO continue until False
  | xsLength > zsLength = isSubsequence (tail xs) zs
  | otherwise           = False
  where
  xsChunk = take zsLength xs
  xsLength = length xs
  zsLength = length zs



balanceLists :: [a] -> [a] -> ([a],[a])
balanceLists a b = (a ++ move, stay) where
  (move, stay) = splitAt (ceiling $ realToFrac (abs (length a - length b)) / (2 :: Float)) b



-- Consider a string. No, consider a different string.

-- We are going to iterate through it. Step by step the values will be as
-- follows (Refer to Minimum Compressable String for why we start from "foobar"):

-- now                prev
--------------------------

-- fooba+r
-- is "bar" in "foo"

-- foobar+f
-- TOO_LONG: "barf" in "foo"?
-- "arf" in "foob"?

-- foobarf+o
-- TOO_LONG: "barfo" in "foo"?
-- "arfo" in "foob"?
-- "rfo" in "fooba"?

-- foobarfo+o
-- TOO_LONG: "barfoo" in "foo"?
-- TOO_LONG: "arfoo" in "foob"?
-- "rfoo" in "fooba"?
-- YES: "foo" in "foobar"?

-- YES: Don't stop yet. Keep taking up until NO.
-- YES: Continue next from here.

-- foobarfoo+b
-- YES: "foob" in "foobar"

-- foobarfoob+a
-- YES: "fooba" in "foobar"

-- foobarfooba+r
-- YES: "foobar" in "foobar"

-- END
-- END_YES: Deal with lingering yes
