import Data.List (intercalate)

main :: IO ()
main = do
  let q = toEnum 34 :: Char
  let c = toEnum 44 :: Char
  let ob = toEnum 91 :: Char
  let cb = toEnum 93 :: Char
  let txt = ["import Data.List (intercalate)","","main :: IO ()","main = do","  let q = toEnum 34 :: Char","  let c = toEnum 44 :: Char","  let ob = toEnum 91 :: Char","  let cb = toEnum 93 :: Char","  let txt = ","  mapM_ putStrLn $ take 8 txt","  putStrLn $ concat [txt !! 8, [ob], [q], intercalate [q,c,q] (take 9 txt ++ drop 9 txt), [q], [cb]]","  mapM_ putStrLn $ drop 9 txt"]
  mapM_ putStrLn $ take 8 txt
  putStrLn $ concat [txt !! 8, [ob], [q], intercalate [q,c,q] (take 9 txt ++ drop 9 txt), [q], [cb]]
  mapM_ putStrLn $ drop 9 txt
