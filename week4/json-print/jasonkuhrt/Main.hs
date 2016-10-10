{- README

-- Code Kata : JSON Print

Define a data type for JSON and print it as JSON (without indentation).
Don't forget that you should support floating-point numbers and escaping in
strings.
-}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import Text.Printf (printf)




data JSON =
    Nil
  | B Bool
  | S String
  | N Double
  | L [JSON]
  | O (Map String JSON)
  deriving (Show)



{- | Stringify a JSON type to a valid JSON string. -}
stringify :: JSON -> String

stringify Nil        = "null"

stringify (B True)   = "true"
stringify (B False)  = "false"

stringify (S string) = stringifyString string

stringify (N number) = stringifyNumber number where
  stringifyNumber :: Double -> String
  stringifyNumber n
    | remainder /= 0 = show n
    | otherwise      = show integer
    where
    (integer, remainder) = properFraction n :: (Integer, Double)

stringify (L list)   =
    wrap "[]"
  . List.intercalate ", "
  . fmap stringify
  $ list

stringify (O object) =
    wrap "{}"
  . List.intercalate ", "
  . Map.elems
  . Map.mapWithKey stringifyKeyValue
  $ object
  where
  stringifyKeyValue :: String -> JSON -> String
  stringifyKeyValue k v =
    stringifyString k ++ ":" ++ stringify v



-- String Handling --

stringifyString :: String -> String
stringifyString = wrap "\"" . escapeString

escapeString :: String -> String
escapeString = go where
  go ""                = ""

  go ('\\' :s)         = "\\\\\\\\"                        ++ go s
  go ('"'  :s)         = "\\\""                            ++ go s

  go ('\b' :s)         = "\\\\b"                           ++ go s
  go ('\f' :s)         = "\\\\f"                           ++ go s
  go ('\n' :s)         = "\\\\n"                           ++ go s
  go ('\r' :s)         = "\\\\r"                           ++ go s
  go ('\t' :s)         = "\\\\t"                           ++ go s

  go (c    :s)
    | Char.isControl c = (printf "\\\\u%04x" . Char.ord) c ++ go s

  go (c    :s)         = c                                  : go s



-- Helpers --

wrap :: String -> String -> String
wrap [end]       s = [end]   ++ s ++ [end]
wrap [start,end] s = [start] ++ s ++ [end]
wrap _           s = s



-- Test --

main :: IO ()
main = putStrLn . stringify $ sample

sample :: JSON
sample =
  O . Map.fromList $ [
    ("1", S "\1foo\nbar\xFFFF"),
    ("a", N 1),
    ("b", N 2.2),
    ("c", O . Map.fromList $ [
      ("ca", S "foo"),
      ("cb", Nil)
    ]),
    ("d", N 2.0),
    ("e", N 2.03),
    ("f", L [N 1, S "bar", Nil, L [], O Map.empty]),
    ("g", S "5 \\ 5"),
    ("h", S "5 \\\\ 5"),
    ("i", S "He said \"foo\"!"),
    ("j", S "\\foobar")
  ]
