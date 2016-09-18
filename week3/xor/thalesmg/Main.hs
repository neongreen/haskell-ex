import System.Environment
import System.IO
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import qualified Data.Bits as B

myXor :: L.ByteString -> BS.ByteString -> BS.ByteString
myXor key text =
  L.toStrict . L.pack $ L.zipWith B.xor (L.cycle key) (L.fromStrict text)
  
main :: IO ()
main = do
  [key, file] <- getArgs
  handle <- openFile file ReadMode
  contents <- BS.hGetContents handle
  print "Starting to XOR file..."
  let byteKey = TLE.encodeUtf8 . TL.pack $ key
      xored = myXor byteKey contents
  print "Going to write..."
  handle2 <- openFile file WriteMode
  BS.hPut handle2 xored
  hClose handle2
  print "Finished XORing file."
