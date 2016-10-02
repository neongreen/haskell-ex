import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.List (sortOn)
import Debug.Trace (trace)

-- |A Node is simply a pair of coordinates.
type Node = (Int, Int)
-- |A Path is actually a Map between a current Node and its previous one.
type Path = M.Map Node Node
-- |A Graph is a map from a Node to 0 or more Nodes, and the distance to them.
type Graph = M.Map Node (Double, [Node])

{-|
  Gets a Graph and a current Node, and returns the list of immediately adjacent
  nodes. This version doesn't allow diagonal movements.
-}
neighbors :: Graph -> Node -> [Node]
neighbors g (x, y) =
  let
    check = flip M.member g 
  in
    [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], 
                        check (x + dx, y + dy), dx * dy == 0, dx + dy /= 0]
                        
{-|
  Gets a Graph and a current Node, and returns the list of immediately adjacent
  nodes. This version permits diagonal movements.
-}
neighbors2 :: Graph -> Node -> [Node]
neighbors2 g (x, y) =
  let
    check = flip M.member g 
  in
    [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], 
                        check (x + dx, y + dy)]

-- |Helper function to read the map into a matrix of 0's and 1's
charTo10 :: Int -> Char -> Int
charTo10 _ char =
  if char == '.' || char == 'A' || char == 'B'
    then 1
    else 0

-- |Infinity.
inf :: Double
inf = 1/0

{-|
  Turns a string representation of a Graph/Maze and turns it into a Graph of
  connected Nodes. It also returns the start and end Nodes. 
  This version does not allow diagonal connections.
-}
readGraph :: [String] -> (Node,  -- ^Start node.
                          Node,  -- ^End node.
                          Graph) -- ^Built graph.
readGraph rows = 
  let
    binMap = map (zipWith charTo10 [0..]) rows
    numRows = length rows
    numCols = length (head rows)
    -- throws exception if the Map does not contain A!
    startNode = head [(x, y) | (row, x) <- zip rows [0..], 
                               ('A', y) <- zip row [0..]]
    endNode = head [(x, y) | (row, x) <- zip rows [0..], 
                               ('B', y) <- zip row [0..]]
    nodes = [(x, y) | (row, x) <- zip binMap [0..], (1, y) <- zip row [0..]]
    graph :: Graph
    graph = M.fromList $ zip nodes (repeat (inf, []))
    filledGraph = foldr (\node acc -> M.insert node (inf, neighbors acc node) acc) graph nodes
  in
    (startNode, endNode, filledGraph)
    
{-|
  Turns a string representation of a Graph/Maze and turns it into a Graph of
  connected Nodes. This version allows diagonal connections.
-}
readGraph2 :: [String] -> (Node,  -- ^Start node.
                          Node,  -- ^End node.
                          Graph) -- ^Built graph.
readGraph2 rows = 
  let
    binMap = map (zipWith charTo10 [0..]) rows
    -- throws exception if the Map does not contain A!
    startNode = head [(x, y) | (row, x) <- zip rows [0..], 
                               ('A', y) <- zip row [0..]]
    endNode = head [(x, y) | (row, x) <- zip rows [0..], 
                               ('B', y) <- zip row [0..]]
    nodes = [(x, y) | (row, x) <- zip binMap [0..], (1, y) <- zip row [0..]]
    graph :: Graph
    graph = M.fromList $ zip nodes (repeat (inf, []))
    filledGraph = foldr (\node acc -> M.insert node (inf, neighbors2 acc node) acc) graph nodes
  in
    (startNode, endNode, filledGraph)

{-|
  Given a connected, unexplored Graph and a starting Node, 
  attempts to find the shortest path using Dijksta's algorithm.
-}
dijkstra :: Graph   -- ^Starting graph
            -> Node -- ^Starting node.
            -> Node -- ^End node.
            -> Path -- ^Resulting path.
dijkstra graph start end =
  let
    updateNode :: (Graph, Path) -> Node -> Node -> Double -> (Graph, Path)
    updateNode (g', p) parent target distance = 
      if distance + delta < oldDistance
        then (M.adjust (\(_, lst) -> (distance + delta, lst)) target g', M.insert target parent p)
        else (g', p) 
      where
        oldDistance = fst $ g' M.! target
        delta = sqrt . fromIntegral $ (fst parent - fst target)^2 + (snd parent - snd target)^2
    go ::  Set Node -- ^Open set 
           -> Set Node -- ^Closed set
           -> (Graph, Path) 
           -> Path
    go open closed (g, path)
      | S.null open = M.empty -- |If there are no nodes in the open set and we
                              --  still didn't return the path, there is no path.
      | otherwise =
        let
          (next, rest) = S.deleteFindMin open
          closed' = S.insert next closed
          (currDist, neighs) = g M.! next
          (g', p) = foldl (\acc n -> updateNode acc next n currDist) (g, path) neighs
          open' = foldr S.insert rest neighs `S.difference` closed 
        in
          if end `M.member` p then p else go open' closed' (g', p)
    startGraph = updateNode (graph, M.empty) start start 0
    --   allNodes = M.keys (fst startGraph)
    in 
      go (S.singleton start) S.empty startGraph
    
{-|
  Given a target end Node and a map of Paths, returns the list of steps needed
  to get from the start Node to the end Node.
-}
getPath :: Node -> Path -> [Node]
getPath end paths
  | M.null paths = []
  | otherwise =
    let
      go :: Node -> [Node] -> [Node]
      go current acc = if paths M.! current == current 
                         then current:acc
                         else go previous (current:acc)
                       where
                         previous = paths M.! current
    in
      go end []
      
{-|
  Given a Path calculated by getPath and the original String map,
  returns another String map with the path drawn into it.
-}
drawPath :: [Node] -> [String] -> [String]
drawPath path rawMap =
  [[changeChar c (x, y) | (c, y) <- zip row [0..]] | (row, x) <- zip rawMap [0..]]
  where
    changeChar c node = if node `elem` path && c /= 'A' && c /= 'B'
                            then '+' 
                            else c

main :: IO ()
main = do
  rawMap <- lines <$> readFile "sample2"
  mapM_ putStrLn rawMap
  putStrLn ""
  let (start, end, graph) = readGraph rawMap
      processedGraph = dijkstra graph start end
      path = getPath end processedGraph
  putStrLn "Without diagonals:"
  if null path
    then putStrLn "There is no path!!!"
    else mapM_ putStrLn $ drawPath path rawMap
  putStrLn ""
  putStrLn "With diagonals:"
  let (start, end, graph2) = readGraph2 rawMap
      processedGraph2 = dijkstra graph2 start end
      path2 = getPath end processedGraph2
  if null path2
    then putStrLn "There is no path!!!"
    else mapM_ putStrLn $ drawPath path2 rawMap
