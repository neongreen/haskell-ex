import Data.List
import Data.Ord
import Data.Maybe

type Patch = (Int, Int)
type Code = Either String Patch

tryCompress :: String -> String -> Maybe Patch
tryCompress xs ys = createPatch lol
  where 
    lol = find (`isPrefixOf` xs) $ subStrings ys
    createPatch :: Maybe String -> Maybe Patch
    createPatch Nothing = Nothing
    createPatch (Just x) = Just (index, length x)
      where index = fromJust $ findSubstringIndex x ys

subStrings :: String -> [String]
subStrings xs =  reverse $ sortOn length $ filter (`isInfixOf` xs) $ filter long $ nub $ subsequences xs
  where 
    long x = length x > 2

findSubstringIndex :: Eq a => [a] -> [a] -> Maybe Int
findSubstringIndex pat str = findIndex (isPrefixOf pat) $ tails str
