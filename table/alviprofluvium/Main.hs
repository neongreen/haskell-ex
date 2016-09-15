import Data.List

spacing :: [[String]] -> [Int]
spacing = map (maximum . width) . transpose
  where
  width :: [String] -> [Int]
  width = map length

format :: [String] -> [String]
format lines = map (concat . zipWith spaced (spacing table)) table
  where 
  table :: [[String]]
  table = map (map (show . (read::String -> Int)) . words) lines

  spaced :: Int -> String -> String
  spaced width xs = take (width + 1) (xs ++ repeat ' ')

getLines :: IO [String]
getLines = takeWhile (not . null) . lines <$> getContents

main :: IO ()
main = do
  putStrLn "Enter a table:"
  lines <- getLines
  mapM_ putStrLn (format lines)
