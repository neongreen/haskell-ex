import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Numeric (showHex)


data JSON
  = JNull
  | JBool Bool
  | JNumber Double
  | JString String
  | JArray [JSON]
  | JObject (Map String JSON)
  deriving (Eq, Ord, Show)

encode :: JSON -> String
encode  JNull = "null"
encode (JBool b) = if b then "true" else "false"
encode (JNumber x) = show x
encode (JString x) = escape x
encode (JArray xs) = "[" ++ intercalate "," (map encode xs) ++ "]"
encode (JObject m) =
  "{" ++ intercalate "," (map showPair (M.toList m)) ++ "}"
  where showPair (k,v) = escape k ++ ":" ++ encode v

escape :: String -> String
escape s = "\"" ++ concatMap escapeChar s ++ "\""
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\b' = "\\b"
    escapeChar '\f' = "\\f"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar x | x < '\x20' = escapeUnicode x
    escapeChar x = [x]

escapeUnicode :: Char -> String
escapeUnicode c = "\\u" ++ replicate (4-length h) '0' ++ h
  where
    h = showHex (fromEnum c) ""

main :: IO ()
main = do
  -- Examples stolen from @thalesmg's solution
  putStrLn . encode $ JNull
  putStrLn . encode $ JBool True
  putStrLn . encode $ JNumber 10.123
  putStrLn . encode $ JString "hello"
  putStrLn . encode $ JArray [JNull, JBool False, JArray []]
  putStrLn . encode $ JObject M.empty
  putStrLn . encode $ JObject $ M.fromList [
    ("fi\"eld1\n", JNull), ("field2\x5", JString ""),
    ("field3", JObject $ M.singleton "subField" (JArray [])) ]
