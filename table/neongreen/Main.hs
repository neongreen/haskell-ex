{-# LANGUAGE ScopedTypeVariables #-}

import Data.List

main = do
  putStrLn "Enter a table:"
  table :: [[Int]] <- map (map read . words) <$> readUntilBlank
  putStr . unlines . map (intercalate " ") $ padTable (map (map show) table)

readUntilBlank :: IO [String]
readUntilBlank = do
  line <- getLine
  if null line then return [] else (line:) <$> readUntilBlank

pad :: Int -> String -> String
pad n s = replicate (n - length s) ' ' ++ s

padTable :: [[String]] -> [[String]]
padTable table = map padRow table
  where
    columns = transpose table
    columnWidths = map (maximum . map length) columns
    padRow row = zipWith pad columnWidths row
