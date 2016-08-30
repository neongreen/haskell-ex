import Data.Char

charValue :: Char -> Int
charValue c
  | isAscii c && isLetter c = ord (toLower c) - ord 'a' + 1
  | otherwise               = 0

isScary :: String -> Bool
isScary xs = sum (map charValue xs) == 13

main :: IO ()
main = do
  ws <- words <$> readFile "/usr/share/dict/words"
  putStrLn $ unwords (filter isScary ws)
