import qualified Data.Map.Lazy as M
import Data.List (sortOn)

-- data Direction = S | SE | E | NE | N | NW | W | SW

type Node = (Int, Int)
type Path = M.Map Node Node

-- A Graph is a map from a Node to 0 or more Nodes, and the distance to them.
type Graph = M.Map Node (Double, [Node])

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

dijkstra :: Graph -> Node -> Path
dijkstra graph start =
  let
    updateNode :: (Graph, Path) -> Node -> Node -> Double -> (Graph, Path)
    updateNode (g', p) parent target distance = 
      if distance + 1 < oldDistance
        then (M.adjust (\(_, lst) -> (distance + 1, lst)) target g', M.insert target parent p)
        else (g', p) 
      where
        oldDistance = fst $ g' M.! target
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
    
printPath :: Node -> Path -> [Node]
printPath end path =
  let
    go :: Node -> [Node] -> [Node]
    go current acc = if path M.! current == current 
                       then current:acc
                       else go previous (current:acc)
                     where
                       previous = path M.! current
  in
    go end []
      
    
neighbors :: Graph -> Node -> [Node]
neighbors g (x, y) =
  let
    check = flip M.member g 
  in
    [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1], check (x', y')]

readSample = do
  rows <- lines <$> readFile "sampleMap.txt"
  let mock = readGraph rows
  return mock

main :: IO ()
main = do
  rows <- lines <$> readFile "sampleMap.txt"
  mapM_ print rows
  putStrLn ""
  let mock = readGraph rows
  -- mapM_ print mock
  print $ neighbors mock (10, 10)
  -- print mock
