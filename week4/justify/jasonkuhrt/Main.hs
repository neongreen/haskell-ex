{- README

-- Justify

Given a string, format it to into left-justified lines of N length.

--- Example

Disable your editor's line-wrap to see this correctly

Input:

    65
    It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness...

Output:

    It was the best of  times, it was the worst  of times, it was the
    age of wisdom, it was the age of foolishness, it was the epoch of
    belief, it was  the epoch of  incredulity, it  was the  season of
    Light, it was the season of Darkness...

--- Notes

* Do not hyphenate words.

* Bonus, try to choose positions of spaces nicely. For example
  in the first line there are 14 spaces, and 2 of them have to be double if we
  want to make the line be 65 characters long; we could choose first and
  second space for that, but instead we chose 5th and 10th so that the
  lengths of word groups would be approximately equal.

* Bonus, handle single words that exceed a line length.
-}

import Data.List

{- | Left justify a paragraph (list of lines).

The last line is not justified. Neither is a single line.

@
> justify 65 "Hello world."

Hello world.
@

In the following a paragraph of 4 lines are justified. Note:

  * First line stretches by two spaces
  * Second line does not have to stretch at all
  * Third line stretches by 4 spaces

@
> justify 65 "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness..."

It was the best of  times, it was the worst  of times, it was the
age of wisdom, it was the age of foolishness, it was the epoch of
belief, it was  the epoch of  incredulity, it  was the  season of
Light, it was the season of Darkness...
@
-}
justify :: Int -> String -> String
justify width linesAll =
  unlines (linesJustified ++ lineLast)
  where
  linesJustified        = fmap (justifyLine width) linesInit
  (linesInit, lineLast) = (splitAtLast . lines) linesAll



{- | Justify a single line to given width.

For example

@
> justifyLine 65 "belief, it was the epoch of incredulity, it was the season of"

belief, it was  the epoch of  incredulity, it  was the  season of
@

Observe:

@
belief, it was the epoch of incredulity, it was the season of
belief, it was  the epoch of  incredulity, it  was the  season of
              \             \                \        \
               3 (+3)        6 (+3)           8 (+2)   10 (+2)
@

  * Stretches are distributed equidistantly relative to one another and edges.
  * Distance is measured by word count.
-}
justifyLine :: Int -> String -> String
justifyLine w line
  | werdCount == 1 = line
  | otherwise      = closeGap generalIncrease indexIncreases werds
  where
  -- Calculate where the remaining increase will be applied
  indexIncreases = distribute residualIncrease werdCount
  -- Evenly spread the effort to close gap across all spaces
  -- `generalIncrease` is the increase common to all spaces.
  -- `residualIncrease` is the remaining increase to distribute.
  (generalIncrease, residualIncrease) = divMod gap (werdCount - 1)
  gap       = w - length line
  werdCount = length werds
  werds     = words line

  closeGap generalIncrease indexIncreases werds =
    intercalate (replicate (1 + generalIncrease) ' ') .
    fmap (\(i, werd) -> werd ++ if elem i indexIncreases then " " else "") .
    zip [0..] $
    werds



{- | Calculate n evenly spaced/sized chunks across given length. -}
distribute :: Int -> Int -> [Int]
distribute _ 0 = []
distribute divCount len =
  drop 1 $ scanl (\x acc -> acc + x) (-1) sizes
  where
  sizes = zipWith (+) (replicate divCount divSize) (padRight 0 (replicate r 1) divCount)
  -- +1 phantom division pushes away from edge
  (divSize, r) = divMod len (divCount + 1)



{- | Chunk a string into lines with the following rules:

* No line exceeds given width, but may be shorter.
* No word is broken across two lines.
-}
setParagraph :: Int -> String -> String
setParagraph w string
  -- Stop once the string is shorter than a single line.
  | w >= length string = string
  | otherwise          =
    let (nextLine, string') = splitAtNextLine w string
     in nextLine ++ "\n" ++ setParagraph w string'



-- | Find the longest line of whole words that do not exceed limit.
splitAtNextLine :: Int -> String -> (String, String)
splitAtNextLine w string = go werd werds where

  (werd:werds) = words string

  go :: String -> [String] -> (String, String)
  go line []              = (line, "")
  go line (werd:werds)
    | length nextLine > w = (line, unwords (werd:werds))
    | otherwise           = go nextLine werds
    where
    nextLine = line ++ " " ++ werd



-- Helpers --

splitAtLast :: [a] -> ([a],[a])
splitAtLast xs = splitAt (length xs - 1) xs

padRight :: a -> [a] -> Int -> [a]
padRight filler xs size =
  xs ++ (fillers n)
  where
  fillers = flip replicate filler
  n = size - length xs



-- Meta --

sample = "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness..."

sampleLong = replicate 66 '-' ++ " " ++ sample

demo :: IO ()
demo = do
  putStrLn $ (justify 65 . setParagraph 65) sample
  putStrLn $ (justify 65 . setParagraph 65) sampleLong
