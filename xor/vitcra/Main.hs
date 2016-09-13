module Main where

import           Data.Bits          (xor)
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import           Data.Text.Encoding (encodeUtf8)
import           System.Environment (getArgs)

encode :: ByteString -> ByteString -> ByteString
encode key str = BS.pack $ BS.zipWith xor str repeatedkey where
  ls = BS.length str
  lk = BS.length key
  repeatedkey = BS.concat $ replicate (1 + div ls lk) key

main :: IO ()
main = do
  [filepath, key] <- getArgs
  str <- BS.readFile filepath
  BS.writeFile filepath $ encode (encodeUtf8 $ T.pack key) str
