import System.IO

main = do
  handle   <- openFile "/usr/share/dict/words" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
