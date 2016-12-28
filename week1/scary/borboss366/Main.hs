module Main where
import Data.Char (toLower, ord, isAsciiLower)

isScary :: String -> Bool
isScary word = countSum word == 13

countSum :: String -> Int
countSum = sum . map (\c -> ord c - ord 'a' + 1) . filter isAsciiLower . map toLower

main :: IO ()
main = processFile "/usr/share/dict/words"

processFile inputFile = do
  input <- readFile inputFile
  print "Scary words:"
  mapM_ print $ filter isScary $ lines input
