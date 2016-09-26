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

sample :: String
sample = "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness..."

demo :: IO ()
demo = putStrLn $ (leftJustify 65 . setParagraph 65) sample



type Paragraph = String

leftJustify :: Int -> Paragraph -> Paragraph
leftJustify w p = unlines ((fmap justify initLines) ++ lastLine)
  where
  (initLines, lastLine) = (splitAtLast . lines) p
  justify line = case findIndex (== ' ') line of
    Just i  -> insertAt i (replicate diff ' ') line
    Nothing -> line
    where
    -- Calculate difference between desired width and actual width.
    -- This difference is the number of spaces that need to be added.
    diff = w - length line




{- | Chunk a string into lines with the following rules:

* No line exceeds given width, but may be shorter.
* No word is broken across two lines.
-}
setParagraph :: Int -> String -> Paragraph
setParagraph w string
  -- Stop once the string is shorter than a single line.
  | w >= length string = string
  | otherwise          =
    let (nextLine, rest) = splitAt (nextLineWidth w string) string
     in nextLine ++ "\n" ++ setParagraph w (drop 1 rest) -- Drop the space



-- | Find the longest line of whole words that do not exceed limit.
nextLineWidth :: Int -> String -> Int
nextLineWidth = closestBefore ' '
-- Solution is to find a limit followed by a space. Since limit is
-- 1-based but closestBefore takes an index, we are searching
-- the trailing character of limit.



-- Helpers --

-- | Find elem instance closest before index.
closestBefore :: Eq a => a -> Int -> [a] -> Int
closestBefore target i xs =
  if   xs !! i == target
  then i
  else closestBefore target (i - 1) xs



insertAt :: Int -> [a] -> [a] -> [a]
insertAt i new existing =
  before ++ new ++ after
  where
  (before, after) = splitAt i existing



splitAtLast :: [a] -> ([a],[a])
splitAtLast xs = splitAt (length xs - 1) xs
