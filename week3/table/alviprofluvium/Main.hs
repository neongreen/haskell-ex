import Data.List

spacing :: [[String]] -> [Int]
spacing = map (maximum . width) . transpose
  where
  width :: [String] -> [Int]
  width = map length

format :: [String] -> [String]
format lines = map (unwords . zipWith spaced (spacing table)) table
  where 
  table :: [[String]]
  table = map (map (show . (read::String -> Int)) . words) lines

  spaced :: Int -> String -> String
  spaced width xs = replicate (width - length xs) ' ' ++ xs

getLines :: IO [String]
getLines = do
  line <- getLine
  if null line then return [] else (line:) <$> getLines

main :: IO ()
main = do
  putStrLn "Enter a table:"
  lines <- getLines
  mapM_ putStrLn (format lines)
