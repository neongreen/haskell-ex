module Main where

import Data.Char (chr)

quote s = chr 34 : s ++ [chr 34]
sp1 = ([chr 32, chr 32, chr 32, chr 32] ++)
sp2 = ([chr 32, chr 32, chr 44, chr 32] ++)
h = 10

self = [
    "module Main where"
  , ""
  , "import Data.Char (chr)"
  , ""
  , "quote s = chr 34 : s ++ [chr 34]"
  , "sp1 = ([chr 32, chr 32, chr 32, chr 32] ++)"
  , "sp2 = ([chr 32, chr 32, chr 44, chr 32] ++)"
  , "h = 10"
  , ""
  , "self = ["
  , "  ]"
  , ""
  , "main :: IO ()"
  , "main = do"
  , "  mapM_ putStrLn (take h self)"
  , "  putStrLn $ sp1 . quote $ head self"
  , "  mapM_ (putStrLn . sp2 . quote) $ tail self"
  , "  mapM_ putStrLn (drop h self)"
  ]

main :: IO ()
main = do
  mapM_ putStrLn (take h self)
  putStrLn $ sp1 . quote $ head self
  mapM_ (putStrLn . sp2 . quote) $ tail self
  mapM_ putStrLn (drop h self)
