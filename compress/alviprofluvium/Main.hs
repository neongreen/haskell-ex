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
    viableSubstrings = filter (`isInfixOf` xs) $ filter long $ subsequences xs


findSubstringIndex :: String -> String -> Maybe Int
findSubstringIndex pat str = findIndex (isPrefixOf pat) $ tails str

loop :: String -> String -> [Code]
loop _ [] = []
loop comp uncomp@(x:xs) 
  | isNothing compresable = Left [x] : loop (comp ++ [x]) xs
  | otherwise             = Right (fromJust compresable) : loop (comp ++ take len uncomp) (drop len uncomp)
  where
    compresable = tryCompress uncomp comp
    (_, len)    = fromJust compresable

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

