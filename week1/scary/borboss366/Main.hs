module Main where
import System.IO (openFile)
import Data.Char (toLower, ord, isAsciiLower)

main :: IO ()
main = processFile "/usr/share/dict/words"

processFile inputFile = do
  input <- readFile inputFile
  let count = length $ filter isScary $ lines input
  putStrLn $ "Total scary words " ++ show count

isScary :: String -> Bool
isScary word = countSum word == 13

countSum :: String -> Int
countSum = sum . map (\c -> ord c - ord 'a' + 1) . filter isAsciiLower . map toLower
