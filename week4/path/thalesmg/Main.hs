import qualified Data.Map.Lazy as M
import Data.List (sortOn)

-- data Direction = S | SE | E | NE | N | NW | W | SW

type Node = (Int, Int)
type Path = M.Map Node Node

-- A Graph is a map from a Node to 0 or more Nodes, and the distance to them.
type Graph = M.Map Node (Double, [Node])

neighbors :: Graph -> Node -> [Node]
neighbors g (x, y) =
  let
    check = flip M.member g 
  in
    [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], 
                        check (x + dx, y + dy), dx * dy == 0, dx + dy /= 0]
                        
neighbors2 :: Graph -> Node -> [Node]
neighbors2 g (x, y) =
  let
    check = flip M.member g 
  in
    [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], 
                        check (x + dx, y + dy)]

readGraph :: [String] -> Graph
readGraph rows = 
  let
    charTo10 :: Int -> Char -> Int
    charTo10 _ char =
      if char == '.' || char == 'A' || char == 'B'
        then 1
        else 0
    inf = 1/0
    binMap = map (zipWith charTo10 [0..]) rows
    nodes = [(x, y) | (row, x) <- zip binMap [0..], (1, y) <- zip row [0..]]
    graph :: Graph
    graph = M.fromList $ zip nodes (repeat (inf, []))
    filledGraph = foldr (\node acc -> M.insert node (inf, neighbors acc node) acc) graph nodes
  in
    filledGraph
    
readGraph2 :: [String] -> Graph
readGraph2 rows = 
  let
    charTo10 :: Int -> Char -> Int
    charTo10 _ char =
      if char == '.' || char == 'A' || char == 'B'
        then 1
        else 0
    inf = 1/0
    binMap = map (zipWith charTo10 [0..]) rows
    nodes = [(x, y) | (row, x) <- zip binMap [0..], (1, y) <- zip row [0..]]
    graph :: Graph
    graph = M.fromList $ zip nodes (repeat (inf, []))
    filledGraph = foldr (\node acc -> M.insert node (inf, neighbors2 acc node) acc) graph nodes
  in
    filledGraph

dijkstra :: Graph -> Node -> Path
dijkstra graph start =
  let
    updateNode :: (Graph, Path) -> Node -> Node -> Double -> (Graph, Path)
    updateNode (g', p) parent target distance = 
      if distance + delta < oldDistance
        then (M.adjust (\(_, lst) -> (distance + delta, lst)) target g', M.insert target parent p)
        else (g', p) 
      where
        oldDistance = fst $ g' M.! target
        delta = sqrt . fromIntegral $ (fst parent - fst target)^2 + (snd parent - snd target)^2
    go :: [Node] -> (Graph, Path) -> Path
    go [] (_, path) = path
    go open (g, path) = 
      let
        next:rest = sortOn fst open
        (currDist, neighs) = g M.! next
        g'p = foldl (\acc n -> updateNode acc next n currDist) (g, path) neighs
      in
        go rest g'p
    startGraph = updateNode (graph, M.empty) start start 0
    allNodes = M.keys (fst startGraph)
  in 
    go allNodes startGraph
    
getPath :: Node -> Path -> [Node]
getPath end paths =
  let
    go :: Node -> [Node] -> [Node]
    go current acc = if paths M.! current == current 
                       then current:acc
                       else go previous (current:acc)
                     where
                       previous = paths M.! current
  in
    go end []
      
drawPath :: [Node] -> [String] -> [String]
drawPath path rawMap =
  [[changeChar c (x, y) | (c, y) <- zip row [0..]] | (row, x) <- zip rawMap [0..]]
  where
    changeChar c node = if node `elem` path then '+' else c

main :: IO ()
main = do
  rawMap <- lines <$> readFile "sampleMap.txt"
  let graph = readGraph rawMap
      start = (0,0)
      end = (8,12)
      processedGraph = dijkstra graph start
      path = getPath end processedGraph
  mapM_ putStrLn $ drawPath path rawMap
  putStrLn ""
  let graph2 = readGraph2 rawMap
      processedGraph2 = dijkstra graph2 start
      path2 = getPath end processedGraph2
  mapM_ putStrLn $ drawPath path2 rawMap
