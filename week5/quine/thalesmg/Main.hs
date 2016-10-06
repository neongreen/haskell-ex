import Data.List (intercalate)

main :: IO ()
main = do
  let q = toEnum 34 :: Char
  let txt = ["import Data.List (intercalate)", 
             "", 
             "main :: IO ()", 
             "main = do",
             "  let q = toEnum 34 :: Char",
             "  let txt = ", 
             "  mapM_ putStrLn $ take 4 txt", "  putStrLn $ (txt !! 4) ++ \"[\\\"\" ++ intercalate \"\\\", \\\"\" (take 5 txt ++ drop 5 txt) ++ \"\\\"]\"", "  mapM_ putStrLn $ drop 5 txt"]
  mapM_ putStrLn $ take 5 txt
  putStrLn $ concat [(txt !! 5), "[", [q], intercalate ([q] ++ ", " ++ [q]) (take 6 txt ++ drop 6 txt), [q], "]"]
  mapM_ putStrLn $ drop 6 txt
-- Concat!!!
