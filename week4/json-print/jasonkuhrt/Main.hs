{- TODO
1 Escape nested quotes
2 Escape explicit escapes
-}

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



stringify :: JSON -> String

stringify Nil        = "null"

stringify (B True)   = "true"
stringify (B False)  = "false"

stringify (S string) = wrapQuotes string

stringify (N number) = stringifyNumber number where
  stringifyNumber :: (Show n, Real n) => n -> String
  stringifyNumber n
    | mod' n 1 == 0 = (takeWhile (/= '.') . show) n
    | otherwise     = show n

stringify (L list)   =
  wrap "[]" .
  List.intercalate ", " .
  List.map stringify $
  list

stringify (O object) =
  wrap "{}" .
  List.intercalate ", " .
  Map.elems .
  Map.mapWithKey stringifyKeyValue $
  object
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
    ("f", L [N 1, S "bar", Nil, L [], O Map.empty])
  ]
