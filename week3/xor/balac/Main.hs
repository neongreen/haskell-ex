import System.Environment
import System.IO
import System.IO.Temp
import System.Directory

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Bits
import Data.Word

encrypt :: BL.ByteString -> BS.ByteString -> BL.ByteString
encrypt contents key = BL.pack $ zipWith xor ( BL.unpack contents ) ( cycle $ BS.unpack key )

main :: IO ()
main = do
    [inpFilename, key] <- getArgs
    inpHandle <- openBinaryFile inpFilename ReadMode
    contents <- BL.hGetContents inpHandle
    let encKey  = encodeUtf8 $ T.pack key
        encData = encrypt contents encKey
    (tmpPath, tmpHandle) <- openBinaryTempFile "." "tmpEnc"
    BL.hPut tmpHandle encData
    hClose tmpHandle
    hClose inpHandle
    renameFile tmpPath inpFilename