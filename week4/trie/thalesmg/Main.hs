import qualified Data.Map.Lazy as M
import Control.Monad (forever)
import Data.Map.Lazy (Map)

data Trie a = Empty | Node Bool (Map a (Trie a)) deriving (Show)

singletonTrie :: [a] -> Bool -> Trie a
singletonTrie [] _ = Empty
singletonTrie [c] b = Node b (M.singleton c Empty)
singletonTrie (c:cs) b = Node b (M.singleton c (singletonTrie cs False))

addToTrie :: (Ord a) => Trie a -> [a] -> Trie a
addToTrie t [] = t
addToTrie Empty xs = singletonTrie xs False
addToTrie (Node bool mp) (x:xs) = 
  case M.lookup x mp of
    Nothing -> Node bool (M.insert x (singletonTrie xs False) mp)
    Just Empty -> Node bool (M.insert x (singletonTrie xs True) mp)
    Just t' -> Node bool (M.insert x (addToTrie t' xs) mp)
    
countNodes :: Trie a -> Int
countNodes t =
  let
    count' Empty acc = acc
    count' (Node _ mp) acc = M.foldr count' (acc + M.size mp) mp
  in
    count' t 0
    
-- Shorter, cleaner version
countNodes2 :: Trie a -> Int
countNodes2 Empty = 0
countNodes2 (Node _ mp) = M.size mp + sum(M.map countNodes2 mp)
    
queryTrie :: (Ord a) => Trie a -> [a] -> [[a]]
queryTrie Empty _ = [[]]
queryTrie (Node bool mp) [] =
  if bool then [] : ws else ws
  where
  ws = do
          c <- M.keys mp
          let t = mp M.! c
          rest <- queryTrie t []
          return $ c:rest
queryTrie (Node _ mp) (x:xs) = case M.lookup x mp of
  Nothing -> []
  Just t -> do
              rest <- queryTrie t xs
              return (x : rest)

main :: IO ()
main = do
  ws <- lines <$> readFile "../../../data/words"
  let trie = foldl addToTrie Empty ws
  putStrLn $ "Trie created. There are " ++ show (countNodes2 trie) ++ " nodes."
  forever $ do
    query <- getLine
    putStrLn . unwords $ queryTrie trie query
