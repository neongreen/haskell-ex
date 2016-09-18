numChars :: Int -> Int
numChars = (+1) . truncate . logBase 10 . fromIntegral . abs . minusSign
  where
    minusSign n = if n < 0 then 10*n else n -- to account for the minus sign

numCharsTable :: [[Int]] -> [Int]
numCharsTable entries =
  let
    nums' = map numChars
  in
    foldr1 (zipWith max) $ map nums' entries
    
-- Pads an Int with spaces to the left
padNumber :: Int -- padding 
          -> Int -- number
          -> String
padNumber pad n =
  replicate s ' ' ++ show n
  where
    s = 1 + pad - numChars n -- 1 extra space to separate columns
    
padRow :: [Int] -> [Int] -> String
padRow pads row =
  concatMap (uncurry padNumber) $ zip pads row -- uncurry makes the function act on a tuple

main :: IO ()
main = do
  putStrLn "Enter a table:"
  table <- loop
  putStrLn ""
  let padding = numCharsTable table
      rowToString = padRow padding
  mapM_ (putStrLn . rowToString) table  
  where
    loop :: IO [[Int]]
    loop = do
      row <- map (read :: String -> Int) . words <$> getLine
      if null row 
        then return []
        else do
          row' <- loop
          return $ row : row'
