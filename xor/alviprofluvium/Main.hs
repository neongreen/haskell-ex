import System.IO
import System.Environment
import System.Directory
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C

encrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString
encrypt txt key = BS.pack $ BS.zipWith xor txt rollingKey
  where
    rollingKey = BS.cycle key

main :: IO ()
main = do
  (fileName : key : _) <- getArgs
  handle <- openBinaryFile fileName ReadMode
  contents <- BS.hGetContents handle

  (tempName, tempHandle) <- openBinaryTempFile "." "temp"
  
  BS.hPut tempHandle $ encrypt contents $ C.pack key

  hClose handle
  hClose tempHandle

  removeFile fileName
  renameFile tempName fileName
