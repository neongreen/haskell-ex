{-# LANGUAGE OverloadedStrings #-}
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

import qualified Data.Bits as Bits
import qualified System.Environment as Env
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC8
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Enc



{- | Encrypt a string via key+XOR+key. -}
encrypt :: ByteString -> ByteString -> ByteString
encrypt key contents =
  B.pack (B.zipWith Bits.xor (B.cycle key) contents)

{- | Decrypt a string via key+XOR.

Because of how XOR works decrypt is just `encrypt` run on something
already encrypted. -}
decrypt :: ByteString -> ByteString -> ByteString
decrypt = encrypt



test :: IO ()
test = do
  let key               = "XYZ"
  let contents          = "abracadabra"
  let encryptedExpected = "9;(9:;<88*8"
  let encrypted         = encrypt key contents
  let decrypted         = decrypt key encrypted
  print   encrypted
  print $ encrypted == encryptedExpected
  print   decrypted
  print $ decrypted == contents



-- NOTE Program crashes if:
--      1. Pattern match fails
--      2. File does not exist (also consider case of mixing arg order)
main :: IO ()
main = do
  args <- Env.getArgs
  let [keyString, filePath] = args
  let key = (Enc.encodeUtf8 . Text.pack) keyString
  contents <- B.readFile filePath
  putStrLn "==> Reading File"
  let encrypted = encrypt key contents
  putStrLn "==> Flipping Encryption"
  seq (B.length contents) (B.writeFile filePath encrypted)
  putStrLn "==> Writing File"
  putStrLn "==> Done"
  putStrLn "==> Result:\n"
  BC8.putStrLn encrypted
