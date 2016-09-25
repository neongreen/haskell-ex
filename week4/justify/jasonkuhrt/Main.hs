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
  want to make the line be 65 characters long; we could choose first and second
  space for that, but instead we chose 5th and 10th so that the lengths of word
  groups would be approximately equal.

* Bonus, handle single words that exceed a line length.
-}

sample :: String
sample = "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness..."

demo :: IO ()
demo = putStrLn $ setParagraph 65 sample




setParagraph :: Int -> String -> String
setParagraph w string
  -- Stop once the string is shorter than a single line.
  | w >= length string = string
  | otherwise          =
    let (nextLine, rest) = splitAt (nextLineLength w string) string
     in nextLine ++ "\n" ++ setParagraph w (drop 1 rest) -- Drop the space



-- | Find the longest string of words without exceeding width.
nextLineLength :: Int -> String -> Int
nextLineLength w string = case string !! w of
  -- Solution is to find the longest w trailed by a space.
  -- Since w is 1-based but !! is 0-based, we are setup to search
  -- the trailing character of w of string.
  ' ' -> w
  _   -> nextLineLength (w - 1) string
