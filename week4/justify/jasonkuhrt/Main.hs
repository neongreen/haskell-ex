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

    It was the best of  times, it was the worst  of times, it was the
    age of wisdom, it was the age of foolishness, it was the epoch of
    belief, it  was the epoch of  incredulity, it  was the  season of
    Light, it was the season of Darkness...

    It was the best of  times, it was the worst  of times, it was the
    age of wisdom, it was the age of foolishness, it was the epoch of
    belief, it  was the epoch  of incredulity, it  was the  season of
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

longSample = replicate 66 '-' ++ " " ++ sample

demo :: IO ()
demo = do
  putStrLn $ (justify 65 . setParagraph 65) sample
  putStrLn $ (justify 65 . setParagraph 65) longSample




{- | TODO -}
justify :: Int -> String -> String
justify w p = unlines ((fmap (justifyLine w) initLines) ++ lastLine)
  where
  (initLines, lastLine) = (splitAtLast . lines) p



{- | TODO -}
justifyLine :: Int -> String -> String
justifyLine w line =
  applyIncreases base increaseIndices (0, werds)
  -- show base
  where
  -- Calculate where the remaining increase will be applied
  increaseIndices = distribute (length werds) spread
  -- Spread gap evenly around all spaces
  -- `base` is the increase for all spaces.
  -- `spread` is the remaining increase to distribute to select spaces.
  (base, spread) = divMod gap (length werds - 1)
  werds = words line
  gap   = w - length line



{- | TODO -}
applyIncreases :: Int -> [Int] -> (Int, [String]) -> String
applyIncreases _    _  (_, [w])    = w
applyIncreases base [] (_, (w:ws)) =
    w ++ replicate (base + 1) ' ' ++ applyIncreases base [] (0, ws)
applyIncreases base (inc:incs) (i, (w:ws))
  | inc == i  =
    w ++ replicate (base + 2) ' ' ++ applyIncreases base incs (succ i, ws)
  | otherwise =
    w ++ replicate (base + 1) ' ' ++ applyIncreases base (inc:incs) (succ i, ws)



{- | TODO -}
distribute :: Int -> Int -> [Int]
distribute _ 0 = []
distribute len divCount =
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
splitAtNextLine :: Int -> String -> (String,String)
splitAtNextLine w string =
  go werd (unwords werds)
  where
  ([werd], werds) = splitAt 1 . words $ string
  go :: String -> String -> (String, String)
  go nextLine string
    | length nextLine >= w = (nextLine, string)
    | otherwise            =
      case (elemIndex ' ' string) of
      Nothing -> (nextLine, string)
      Just i  ->
        if length nextLine + i > w
        then
          (nextLine, string)
        else
          let (nextWord, (_:string')) = splitAt (i) string
          in go (nextLine ++ " " ++ nextWord) string'



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



padRight :: a -> [a] -> Int -> [a]
padRight filler xs size =
  xs ++ (fillers n)
  where
  fillers = flip replicate filler
  n = size - length xs
