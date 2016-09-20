{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
--------------------------------------------------------------------------------
-- 18. Trie /{trie}/
--
-- Construct a trie from all words in a dictionary and implement search for
-- words by prefix. Here's an example of a trie for
-- /{cool, cat, coal, bet, bean}/:
--
--         b       c
--        /       / \
--       e       a   o
--      / \     /   / \
--     t   a   t   a   o
--         |       |   |
--         n       l   l
--
-- You should read the words file, construct a trie, say how many nodes are in
-- the trie (e.g. in the sample one there are 13 nodes), and then answer user's
-- queries to find all words starting with given letters:
--
--     Trie created. There are 13 nodes.
--
--     > be
--     bean bet
--
--     > c
--     cat coal cool
--
--     > co
--     coal cool
--
-- You can use the following type for the trie (but feel free to use something
-- else):
--
--     data Trie a = Empty | Node (Map a (Trie a))
--
-- The list of words in available in the /data\// folder in the repository.
--------------------------------------------------------------------------------
module Main where

import Data.Function
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.List
import Debug.Trace
import qualified Data.Map.Strict as M
import qualified Data.Set as S

filepath :: FilePath
filepath = "data/words"

main :: IO ()
main = do
  contents <- readFile filepath
  let t = fromFileContents contents
  putStrLn $ "Trie created. There are " ++ show (size t)  ++ " nodes."
  return ()

data Trie a
  = Empty
  | Node (Map a (Trie a))
  deriving (Show)

instance Ord a => Monoid (Trie a) where
  mempty :: Trie a
  mempty = Empty

  mappend :: Trie a -> Trie a -> Trie a
  mappend Empty Empty = Empty
  mappend Empty t = t
  mappend t Empty = t
  mappend (Node t0) (Node t1) = Node $ M.unionWith (mappend) t0 t1

fromList :: Ord a => [a] -> Trie a
fromList    []  = Empty
fromList (a:as) = Node . M.singleton a . fromList $ as

toList :: Trie a -> [[a]]
toList Empty = [[]]
toList (Node m) = concat . M.elems . M.mapWithKey go $ m
  where
    go :: a -> Trie a -> [[a]]
    go k = fmap (k:) . toList

-- assume that the words are easy to work with
fromFileContents :: String -> Trie Char
fromFileContents = foldl' mappend mempty . fmap fromList . lines

size :: Trie a -> Int
size Empty = 0
size (Node m) = M.size m + sum (M.map size m)

subTrie :: Ord a => Trie a -> [a] -> Maybe (Trie a)
subTrie Empty as = Nothing
subTrie m     [] = Just m
subTrie (Node m) (a:as) = M.lookup a m >>= (\nxt -> subTrie nxt as)



