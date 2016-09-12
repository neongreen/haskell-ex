import Data.List.Safe
import Data.Ord
import Data.Maybe
import Data.Function
import Data.Monoid
import Test.QuickCheck

type Patch = (Int, Int)
type Code = Either String Patch

match :: String -> String -> Maybe Patch
match back front = maximumBy (comparing snd <> comparing (Down . fst)) matches
  where 
    matches = filter long $ map (`createPatch` front) (suffixes back)
    long xs = snd xs > 2

createPatch :: (Int, String) -> String -> Patch
createPatch (i, xs) ys = (i, length common)
  where common = takeWhile (uncurry (==)) $ zip xs ys

suffixes :: String -> [(Int, String)]
suffixes = zip [0..] . tails

compress :: String -> [Code]
compress = flatten . go []
  where
    go :: String -> String -> [Code]
    go _ [] = []
    go back front@(x:xs)
      | isNothing patch = Left [x] : go (back ++ [x]) xs
      | otherwise       = Right (fromJust patch) : go (back ++ compressed) (drop l front)
      where
        patch      = match back front
        (_, l)     = fromJust patch
        compressed = take l front

flatten :: [Code] -> [Code]
flatten [] = []
flatten (Left x:Left y:zs) = flatten $ Left (x++y) : zs
flatten (x:xs) = x : flatten xs

uncompress :: [Code] -> String
uncompress = go []
  where
    go :: String -> [Code] -> String
    go xs [] = xs
    go xs (Left y:ys) = go (xs ++ y) ys 
    go xs (Right (i, l):ys) = go (xs ++ link) ys
      where link = take l $ drop i xs

propCompression :: String -> Property
propCompression xs = (uncompress . compress) xs === xs

main :: IO ()
main = quickCheck propCompression
