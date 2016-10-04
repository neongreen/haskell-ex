import Data.Char
import Data.List
main = do
    let bracket arr = putStr ( intercalate [chr 34] arr ) >> putStrLn ( show arr ) >> putStr "    bracket codeArray"
    let codeArray = ["import Data.Char\nimport Data.List\nmain = do\n    let bracket arr = putStr ( intercalate [chr 34] arr ) >> putStrLn ( show arr ) >> putStr ","    bracket codeArray","\n    let codeArray = "]
    bracket codeArray