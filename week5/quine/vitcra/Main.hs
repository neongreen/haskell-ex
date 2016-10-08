module Main where

import Data.Char (chr)

sp1 = ([chr 32, chr 32, chr 32, chr 32] ++)
sp2 = ([chr 32, chr 32, chr 44, chr 32] ++)
h = 9

self = [
    "module Main where"
  , ""
  , "import Data.Char (chr)"
  , ""
  , "sp1 = ([chr 32, chr 32, chr 32, chr 32] ++)"
  , "sp2 = ([chr 32, chr 32, chr 44, chr 32] ++)"
  , "h = 9"
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
