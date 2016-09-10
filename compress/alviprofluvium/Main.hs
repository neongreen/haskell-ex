import Data.List
import Data.Ord
import Data.Maybe

type Patch = (Int, Int)
type Code = Either String Patch

tryCompress :: String -> String -> Maybe Patch
tryCompress xs ys = createPatch match
  where 
    match = find (`isPrefixOf` xs) $ subStrings ys
    createPatch :: Maybe String -> Maybe Patch
    createPatch Nothing  = Nothing
    createPatch (Just x) = Just (index, length x)
      where index = fromJust $ findSubstringIndex x ys

subStrings :: String -> [String]
subStrings xs =  reverse $ sortOn length viableSubstrings
  where 
    long x           = length x > 2
    viableSubstrings = filter (`isInfixOf` xs) $ filter long $ nub $ subsequences xs


findSubstringIndex :: String -> String -> Maybe Int
findSubstringIndex pat str = findIndex (isPrefixOf pat) $ tails str
