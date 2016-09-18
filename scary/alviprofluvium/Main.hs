import Data.Char

scary :: String -> Int
scary = sum . map value 
  where
    value x
      | isLetter x && isAscii x = ord (toUpper x) - (ord 'A' - 1)
      | otherwise               = 0

isScary :: String -> Bool
isScary xs = scary xs == 13

main :: IO ()
main = do
  contents <- readFile "/usr/share/dict/words"
  putStr $ unlines $ filter isScary $ words contents
  

