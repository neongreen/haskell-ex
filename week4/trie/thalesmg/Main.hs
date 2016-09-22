
import qualified Data.Map.Lazy as M

data Trie a = Empty | Node (M.Map a (Trie a)) deriving (Show)

mkTrie :: [a] -> Trie a
mkTrie [] = Empty
mkTrie (c:cs) = Node (M.singleton c (mkTrie cs))

mergeTries :: Trie a -> Trie a -> Trie a
mergeTries Empty t = t
mergeTries t Empty = t

addToTrie :: (Ord a) => Trie a -> [a] -> Trie a
addToTrie t [] = t
addToTrie Empty xs = mkTrie xs
addToTrie (Node mp) (x:xs) = 
  case M.lookup x mp of
    Nothing -> Node (M.insert x (mkTrie xs) mp)
    Just t' -> Node (M.insert x (addToTrie t' xs) mp)
    
countNodes :: Trie a -> Int
countNodes t =
  let
    count' Empty acc = acc
    count' (Node mp) acc = M.foldr count' (acc + M.size mp) mp
  in
    count' t 0

main :: IO ()
main = return ()
