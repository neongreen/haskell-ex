{- README

-- XOR Encryption

-- Spec

Write a program that encrypts a file by XORing it with a key. For instance,
if the file is “abracadabra” and the key is “XYZ”, then the result would be

     Key  "XYZ"
    File  "abracadabra"
     XOR   XYZXYZXYZXY
       =   9;(9:;<88*8

It should accept the file name and the key as command-line arguments, and
overwrite the file. (Due to the way XOR works, encrypting an already encrypted
file will decrypt it.) Use bytestring to read the file, and encode the key as
UTF8 (with encodeUtf8).

Learn more about XOR Ciphers at en.wikipedia.org/wiki/xor_cipher.
-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Bits as Bits
import qualified Data.ByteString.Char8 as BC8



{- | Encrypt a string via XOR+key. -}
encrypt :: String -> ByteString -> ByteString
encrypt key bs =
  B.pack (B.zipWith Bits.xor (BC8.pack cycledKey) bs)
  where
  cycledKey = take (B.length bs) (cycle key)

decrypt :: String -> String -> String
decrypt = undefined



main :: IO ()
main = do
  let key = "XYZ"
  let contents = BC8.pack "abracadabra"
  let expected = BC8.pack "9;(9:;<88*8"
  print $ encrypt key contents
  print $ encrypt key contents == expected
