import Data.Map (Map)
import Data.List (intercalate)
import qualified Data.Map as M
import Numeric (showHex)

data JSON = JNull | JNum Double | JString String | JArray [JSON] | JObj (Map String JSON) | JBool Bool

escapeUnicode :: Char -> String
escapeUnicode c = "\\u" ++ replicate (4 - length h) '0' ++ h
 where
   h = showHex (fromEnum c) ""

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

instance Show JSON where
  show JNull = "null"
  show (JBool b) = if b then "true" else "false"
  show (JNum x) = show x
  show (JString s) = escape s
  show (JArray objs) = "[" ++ intercalate "," (map show objs) ++ "]"
  show (JObj m) = "{" ++ intercalate "," (elems m) ++ "}"
    where
      elems x = 
        map (\(key,value) -> (escape key ++ ":" ++ show value)) (M.toList x)

main :: IO ()
main = do
  print JNull
  print $ JBool True
  print $ JNum 10.123
  print $ JString "hello"
  print $ JArray [JNull, JBool False, JArray []]
  print $ JObj M.empty
  print $ JObj $ M.fromList [("field1", JNull), ("field2", JString "A \" quote!"),
                              ("field3", JObj $ M.singleton "subField" (JArray []))]
  -- putStrLn "A new\nline!"
  print $ JString "A new\nline!"
