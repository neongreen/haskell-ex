{- README

-- Code Kata : JSON Print

Define a data type for JSON and print it as JSON (without indentation).
Don't forget that you should support floating-point numbers and escaping in
strings.
-}
module Main where

import Data.Fixed (mod')
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List



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

stringify (S string) =
    wrapQuotes
  . escape '\\' -- Otherwise parse yields invalid/different strings. [1]
  . escape '"'  -- Otherwise it would terminate string parsing too early.
  . escape '\\' -- Otherwise it would be an escape during parse
  $ string
{- [1]

The first-level escape preserves slashes/quotes at the string level while
the second-level escape preserves these escapes at the transmission level.
Without escaping escapes parse would yield invalid (or just different)
strings (code). For example:

           READ + BAD                  STRING
IN         STRINGIFY      PARSE        RESULT
-------    ----------     --------     ---------
5\5     -> "5\\5"      -> "5\"      -> "5        (Error: Unexpected number)
"huh"?  -> "\"huh\"?"  -> ""huh"?"  -> ""h ...   (Error: Unexpected token h)
\foobar -> "\\foobar"  -> "\foobar" -> "\foobar" (Different: oobar vs \foobar)
-}

stringify (N number) = stringifyNumber number where
  stringifyNumber :: (Show n, Real n) => n -> String
  stringifyNumber n
    | mod' n 1 == 0 = (takeWhile (/= '.') . show) n
    | otherwise     = show n

stringify (L list)   =
    wrap "[]"
  . List.intercalate ", "
  . List.map stringify
  $ list

stringify (O object) =
    wrap "{}"
  . List.intercalate ", "
  . Map.elems
  . Map.mapWithKey stringifyKeyValue
  $ object
  where
  stringifyKeyValue :: String -> JSON -> String
  stringifyKeyValue k v = wrapQuotes k ++ ":" ++ stringify v



-- Helpers --

wrapQuotes :: String -> String
wrapQuotes = wrap "\""

wrap :: String -> String -> String
wrap [end]       s = [end]   ++ s ++ [end]
wrap [start,end] s = [start] ++ s ++ [end]
wrap _           s = s

escape :: Char -> String -> String
escape cEscape s =
  (\ c -> if c == cEscape then ['\\', c] else [c]) =<< s



-- Test --

main :: IO ()
main = putStrLn . stringify $ sample

sample :: JSON
sample =
  O . Map.fromList $ [
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
