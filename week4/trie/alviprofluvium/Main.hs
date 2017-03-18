module Trie where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as MapL

data Trie a = Empty | Node (Map a (Trie a))
  deriving Show

createTrie :: Ord a => [[a]] -> Trie a
createTrie = foldl addToTrie Empty
  where
  addToTrie :: Ord a => Trie a -> [a] -> Trie a
  addToTrie t []         = t
  addToTrie Empty (x:xs) = Node (MapL.singleton x (addToTrie Empty xs))
  addToTrie (Node m) (x:xs)
    | MapL.member x m = Node (MapL.adjust (`addToTrie` xs) x m)
    | otherwise       = Node (MapL.insert x (addToTrie Empty xs) m)

lenghtTrie :: Trie a -> Int
lenghtTrie Empty    = 0
lenghtTrie (Node m) = sum (MapL.map lenghtTrie m) + MapL.size m

main :: IO ()
main = do
  contents <- readFile "../../../data/words"
  let trie = createTrie $ lines contents
  putStrLn $ "Trie created. There are " ++ (show . lenghtTrie) trie ++ " nodes."
  
  
