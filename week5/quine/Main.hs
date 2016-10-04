import Data.Char
import Data.List
main = do
    let bracket str = putStr ( intercalate [chr 34] str ) >> putStrLn ( show str ) >> putStr "    bracket prefix"
    let prefix = ["import Data.Char\nimport Data.List\nmain = do\n    let bracket str = putStr ( intercalate [chr 34] str ) >> putStrLn ( show str ) >> putStr ","    bracket prefix","\n    let prefix = "]
    bracket prefix