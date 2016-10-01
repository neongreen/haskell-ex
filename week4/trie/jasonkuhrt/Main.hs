
{- README
-- Trie

Construct a trie of all words in a dictionary.
Use it for searching words by prefix.

For example, a trie for words: cool, cat, coal, bet, bean.

        b       c
       /       / \
      e       a   o
     / \     /   / \
    t   a   t   a   o
        |       |   |
        n       l   l
-}
module Main where

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map


{- Trie type.

In our model it might at first seem that one word coresponds to one path
within a trie of letters, but that would be wrong; Paths can actually
naturally represent multiple words since words can also be the prefix of
other words. For example:

     a and
    be bean

As a visualized trie:

      a       b
     /         \
    n           e
                 \
                  a
                  |
                  n

So our solution is to model each Node with a flag indicating whether it
represents a valid word or not at that point in the path. For example, the
above visualization in pseudo code:

    Node False
      a : Node True
        n : Empty
      b : Node False
        e : Node True
          a : Node False
            n : Empty

Finally each letter (a Node) may branch out to zero, one, or many other
letters (nodes) which themselves finish or continue toward another word.
For example:

        b
       /
      e
     /|\
    t e  a
      |  |
      r  n

Our solution is to use a Map data type which permits us to model arbitrary
number of children per node.
-}
data Trie a =
  Empty |
  Node Bool (Map a (Trie a))
  deriving (Show)

sample :: Trie Char
sample =
  Node False (Map.fromList [
    ('a', Node True (Map.fromList [
      ('n', Empty)
    ])),
    ('b', Node False (Map.fromList [
      ('e', Node True (Map.fromList [
        ('a', Node False (Map.fromList [
          ('n', Empty)
        ]))
      ]))
    ]))
  ])



-- | Interactive search on the command line.
main :: IO ()
main = do
  putStr "==> Indexing dictionary... "
  trie <- pure . wordsToTrie . lines =<< readFile "/usr/share/dict/words"
  putStrLn "Done!"
  putStrLn "==> Now search for words by prefix."
  prompt trie
  where
  prompt trie = do
    putStr "> "
    putStrLn . unwords . flip search trie =<< getLine
    putStrLn ""
    prompt trie



{- Index a dictionary of words.

For example:

  > wordsToTrie ["bet", "be", "bean","beam"]

  Node False (fromList [
    ('b', Node False (fromList [
      ('e', Node True (fromList [
        ('t', Empty),
        ('a', Node False (fromList [
          ('m', Empty),
          ('n',Empty)
        ]))
      ]))
    ]))
  ])
-}
wordsToTrie :: [String] -> Trie Char
wordsToTrie = foldl indexWord (Node False Map.empty)

indexWord :: Trie Char -> String -> Trie Char
-- Finished indexing word whose entire path already existed
indexWord (Node _ branches)  ""         = Node True branches
-- Extend a trie path
indexWord Empty     string              = stringToTrie True string
indexWord (Node isWord maap) (char:cs)
  -- Word path already exists
  | Map.member char maap =
    Node isWord (Map.update (Just . flip indexWord cs) char maap)
  -- Word path is novel
  | otherwise =
    Node isWord (Map.insert char (stringToTrie False cs) maap)

stringToTrie :: Bool -> String -> Trie Char
stringToTrie _      ""     = Empty
stringToTrie isWord (c:cs) = Node isWord (Map.singleton c (stringToTrie False cs))



-- | Find words in given trie that have given prefix.
search :: String -> Trie Char -> [String]
search string = trieToWords . findTrieWithPrefix string

{- | Convert a trie into a list of words therein.

TODO Refactor!

For example:

    trie =
      Node False (Map.fromList [
        ('a', Node True (Map.fromList [
          ('n', Empty)
        ])),
        ('b', Node False (Map.fromList [
          ('e', Node True (Map.fromList [
            ('a', Node False (Map.fromList [
              ('n', Empty)
            ]))
          ]))
        ]))
      ])

    > trieToWords trie

    ["a","an","be","bean"]
-}
trieToWords :: Trie Char -> [String]
trieToWords Empty      = []
trieToWords (Node _ m) = concat . Map.elems $ Map.mapWithKey (go "") m
  where
  go :: String -> Char -> Trie Char -> [String]
  go prefix char Empty =
    [prefix ++ [char]]
  go prefix char (Node False maap) =
    concat . Map.elems $
    Map.mapWithKey (go (prefix ++ [char])) maap
  go prefix char (Node True maap) =
    ((prefix ++ [char]) :) . concat . Map.elems $
    Map.mapWithKey (go (prefix ++ [char])) maap

{- | Find the trie matching a given prefix.

The trie returned does include the prefix given but all isWord flags are set
to False. For example given a trie with the four words "be", "beam", "bean",
"bet" and a prefix of "bea" then the result excludes "t" branch as well as
unmarking "be" as a word. Observe:

  trie =
    Node False (fromList [
      ('b', Node False (fromList [
        ('e', Node True (fromList [
          ('t', Empty),
          ('a', Node False (fromList [
            ('m', Empty),
            ('n',Empty)
          ]))
        ]))
      ]))
    ])

    > findTrieWithPrefix "bea" trie

    Node False (fromList [
      ('b', Node False (fromList [
        ('e', Node False (fromList [
          ('a', Node False (fromList [
            ('m', Empty),
            ('n',Empty)
          ]))
        ]))
      ]))
    ])
-}
findTrieWithPrefix :: String -> Trie Char -> Trie Char
findTrieWithPrefix prefix trie = fromMaybe Empty (go prefix trie)
  where
  go _         Empty                  = Nothing
  go ""        node                   = Just node
  go (char:cs) (Node _ branches) =
    case Map.lookup char branches of
      Nothing -> Nothing
      Just t  -> fmap (Node False . Map.singleton char) (go cs t)
