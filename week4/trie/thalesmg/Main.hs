import qualified Data.Map.Lazy as M

data Trie a = Empty | Node (M.Map a (Trie a)) deriving (Show)

mkTrie :: [a] -> Trie a
mkTrie [] = Empty
mkTrie (c:cs) = Node (M.singleton c (mkTrie cs))

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
    
-- Shorter, cleaner version
countNodes2 :: Trie a -> Int
countNodes2 Empty = 0
countNodes2 (Node mp) = M.size mp + sum(M.map countNodes2 mp)
    
queryTrie :: (Ord a) => Trie a -> [a] -> [[a]]
queryTrie Empty _ = [[]]
queryTrie (Node mp) [] =
  do
    c <- M.keys mp
    let t = mp M.! c
    rest <- queryTrie t []
    return $ c : rest
queryTrie (Node mp) (x:xs)
  | x `M.notMember` mp = []
  | otherwise = 
      do
        let t = mp M.! x
        rest <- queryTrie t xs
        return (x : rest)

main :: IO ()
main = do
  ws <- lines <$> readFile "../../../data/words"
  let trie = foldl addToTrie Empty ws
  putStrLn $ "Trie created. There are " ++ show (countNodes2 trie) ++ " nodes."
  loop trie
  where
    loop t = do
      query <- getLine
      putStrLn . unwords $ queryTrie t query
      loop t
