
type CompressedChunk = Either String (Int, Int)

decompress :: [CompressedChunk] -> String
decompress chunks =
    let
        decomp :: String -> CompressedChunk -> String
        decomp acc c = 
            acc ++ expanded c
            where 
                expanded chunk = case chunk of
                    Left str -> str
                    Right (start, len) -> take len . drop start $ acc
    in
        foldl decomp "" chunks

decompressInput :: [CompressedChunk]
decompressInput = [Left "Consider a string. No, c",Right (1,10),Left "different",Right (10,9),Left "Whatever."]

main :: IO ()
main = return ()