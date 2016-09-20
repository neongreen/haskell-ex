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
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL



{- | CLI to de/encrypt a file.

xor KEY FILEPATH

Multiple runs on the same file toggles encryption due to the nature of XOR.
NOTE Crashes if CLI arguments are not given correctly.

Example:
@
> xor blah ~/test-file.md

==> Reading File
==> Flipping Encryption
==> Writing File
==> Done
==> Result:

HMkL2
@
-}
main :: IO ()
main = do
  [keyString, filePath] <- Env.getArgs
  let key = (TL.encodeUtf8 . TL.pack) keyString
  endecryptFile key filePath



{- | De/encrypt a file.

NOTE Crashes if file does not exist -}
endecryptFile :: ByteString -> String -> IO ()
endecryptFile key filePath = do
  putStrLn "==> Reading File"
  contents <- BSL.readFile filePath
  putStrLn "==> Flipping Encryption"
  let encrypted = encrypt key contents
  putStrLn "==> Writing File"
  -- Force the lazily read file to be read otherwise we will attempt to write
  -- over a file before we've even read it! For example on OSX there is an
  -- error stating the file is locked (because OS locks the file on read).
  seq (BSL.length contents) (BSL.writeFile filePath encrypted)
  putStrLn "==> Done"
  putStrLn "==> Result:\n"
  BSL.putStrLn encrypted



{- | Encrypt a string via key+XOR+key. -}
encrypt :: ByteString -> ByteString -> ByteString
encrypt key contents =
  BSL.pack (BSL.zipWith Bits.xor (BSL.cycle key) contents)

{- | Decrypt a string via key+XOR.

Because of how XOR works decrypt is just `encrypt` run on something
already encrypted. -}
decrypt :: ByteString -> ByteString -> ByteString
decrypt = encrypt



{- | Minimal de/encryption test. -}
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
