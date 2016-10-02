import Data.Map (Map)
import Data.List (intercalate)
import qualified Data.Map as M

data JSON = JNull | JNum Double | JString String | JArray [JSON] | JObj (Map String JSON) | JBool Bool

instance Show JSON where
  show JNull = "null"
  show (JBool b) = if b then "true" else "false"
  show (JNum x) = show x
  show (JString s) = "\"" ++ escape s ++ "\""
    where
      escape =
        foldr (\c -> (++) (if c == '\"' then "\\\"" else [c])) []
  -- show (JArray []) = "[]"
  show (JArray objs) = "[" ++ intercalate "," (map show objs) ++ "]"
  show (JObj m) = "{" ++ intercalate "," (elems m) ++ "}"
    where
      elems x = 
        map (\(key,value) -> ("\"" ++ key ++ "\":" ++ show value)) (M.toList x)

main :: IO ()
main = do
  print JNull
  print $ JBool True
  print $ JNum 10.123
  print $ JString "hello"
  print $ JArray [JNull, JBool False, JArray []]
  print $ JObj M.empty
  print $ JObj $ M.fromList [("field1", JNull), ("field2", JString "A \" quote!"),
                              ("field3", JObj $ M.singleton "subField" (JArray [])) ]
