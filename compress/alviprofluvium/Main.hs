import Data.List
import Data.Ord
import Data.Maybe
import Data.Function

type Patch = (Int, Int)
type Code = Either String Patch

tryCompress :: String -> String -> Maybe Patch
tryCompress xs ys = createPatch $ matches xs ys
  where 
    createPatch :: Maybe String -> Maybe Patch
    createPatch Nothing  = Nothing
    createPatch (Just x) = Just (index, length x)
      where index = fromJust $ findSubstringIndex x ys

matches :: String -> String -> Maybe String
matches xs ys = find (`isPrefixOf` xs) viableSubstrings
  where 
    long x = length x > 2
    viableSubstrings = reverse $ sortOn length $ filter long $ getSubstrings xs ys

--Source
--https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Longest_common_substring#Haskell
getSubstrings :: String -> String -> [String]
getSubstrings xs ys = concat $ [f xs' ys | xs'<- tails xs] ++ [f xs ys' | ys' <- drop 1 $ tails ys]
  where f xs ys = scanl g [] $ zip xs ys
        g z (x,y) = if x == y then z ++ [x] else []


findSubstringIndex :: String -> String -> Maybe Int
findSubstringIndex pat str = findIndex (isPrefixOf pat) $ tails str

loop :: String -> String -> [Code]
loop _ [] = []
loop comp uncomp@(x:xs) 
  | isNothing compression = Left [x] : loop (comp ++ [x]) xs
  | otherwise             = Right (fromJust compression) : loop (comp ++ take len uncomp) (drop len uncomp)
  where
    compression = tryCompress uncomp comp
    (_, len)    = fromJust compression

compress :: String -> [Code]
compress = flatten . loop []

flatten :: [Code] -> [Code]
flatten [] = []
flatten (Left x:Left y:zs) = flatten $ Left (x++y) : zs
flatten (x:xs) = x : flatten xs

uncompressLoop :: [Code] -> String -> String
uncompressLoop [] ys = ys
uncompressLoop (Right (index, length):xs) ys = uncompressLoop xs $ ys ++ link 
  where link = take length $ drop index ys
uncompressLoop (Left x:xs) ys = uncompressLoop xs $ ys ++ x

uncompress :: [Code] -> String
uncompress xs = uncompressLoop xs []

testCompression :: String -> Bool
testCompression xs = (uncompress . compress) xs == xs

