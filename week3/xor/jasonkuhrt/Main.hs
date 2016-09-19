{- README

-- XOR Encryption

-- Spec

Write a program that encrypts a file by XORing it with a key. For instance:

     Key  "XYZ"
Contents  "abracadabra"
     XOR   XYZXYZXYZXY
       =   9;(9:;<88*8

* It should accept the file name and the key as command-line arguments
* Use bytestring to read the file
* Encode the key as UTF8 (with encodeUtf8)
* Overwrite the file with encryped variant

-- Notes

Due to the way XOR works, encrypting an already encrypted file will decrypt
it. Learn more about XOR Ciphers at en.wikipedia.org/wiki/xor_cipher.
-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Bits as Bits
import qualified Data.ByteString.Char8 as BC8



{- | Encrypt a string via key+XOR+key. -}
encrypt :: String -> ByteString -> ByteString
encrypt key bs =
  B.pack (B.zipWith Bits.xor (BC8.pack cycledKey) bs)
  where
  cycledKey = take (B.length bs) (cycle key)

{- | Decrypt a string via key+XOR. -}
decrypt :: String -> ByteString -> String
decrypt key bs = BC8.unpack (encrypt key bs)



test :: IO ()
test = do
  let key               = "XYZ"
  let contents          = BC8.pack "abracadabra"
  let encryptedExpected = BC8.pack "9;(9:;<88*8"
  let encrypted         = encrypt key contents
  let decrypted         = decrypt key encrypted
  print   encrypted
  print $ encrypted == encryptedExpected
  print   decrypted
  print $ decrypted == BC8.unpack contents



main :: IO ()
main = undefined
-- TODO
-- * It should accept the file name and the key as command-line arguments
-- * Use bytestring to read the file
-- * Encode the key as UTF8 (with encodeUtf8)
-- * Overwrite the file with encryped variant
