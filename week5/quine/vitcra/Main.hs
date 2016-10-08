module Main where

sp1 = ("    " ++)
sp2 = ("  , " ++)
h = 7

self = [
    "module Main where"
  , ""
  , "sp1 = (\"    \" ++)"
  , "sp2 = (\"  , \" ++)"
  , "h = 7"
  , ""
  , "self = ["
  , "  ]"
  , ""
  , "main :: IO ()"
  , "main = do"
  , "  mapM_ putStrLn (take h self)"
  , "  putStrLn $ sp1 . show $ head self"
  , "  mapM_ (putStrLn . sp2 . show) $ tail self"
  , "  mapM_ putStrLn (drop h self)"
  ]

main :: IO ()
main = do
  mapM_ putStrLn (take h self)
  putStrLn $ sp1 . show $ head self
  mapM_ (putStrLn . sp2 . show) $ tail self
  mapM_ putStrLn (drop h self)
