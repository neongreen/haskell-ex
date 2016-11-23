import System.IO
import Data.Char

magickNumber = 13
fPath = "/usr/share/dict/words"

isScary :: [Char] -> Bool
isScary word
    | isAlphaOnly word && foldl (\acc c -> acc + ord (toUpper c) - ord 'A' + 1) 0 word == magickNumber = True
    | otherwise = False
    where isAlphaOnly = all (isAlpha)

main = do
    withFile fPath ReadMode (\handle -> do
        content <- hGetContents handle
        print $ filter isScary $ lines content)
