-- | Wilson's algorithm

{-# LANGUAGE UnicodeSyntax #-}

module Wilson
  where

data Node = Node Int Int
data Tree = EmptyTree | Leaf Node | Tree [Tree]
data Edge = Edge Node Node

type Source = Node
type Target = Node

type Positon = (Int, Int)

data Step  = Step { movX :: Int, movY :: Int } -- like a offset

offset ∷ [Step]
offset =
  [ Step { movX = 1, movY = 0 } --right
  , Step { movX = 0, movY = 1 } --up
  , Step { movX = -1, movY = 0 } --left
  , Step { movX = 1, movY = -1 } --down
  ]

type Steps = [Step]
type Table = [Node]


-- Description of the Wilson's algorithm:
--
-- Choose any vertex at random and add it to the UST.
-- Select any vertex that is not already in the UST and perform a random walk until you encounter a vertex that is in the UST.
-- Add the vertices and edges touched in the random walk to the UST.
-- Repeat 2 and 3 until all vertices have been added to the UST.

getMaze :: Table → Tree → Tree
getMaze table ust = undefined
  -- node = randomNode table
  -- check node is not in ust
  -- newUST = addNode node ust
  -- node2 = randomNode table
  -- vertex that is not already in the UST
  -- ..


addNode :: Node → Tree → Tree
addNode = undefined

addEdge ∷ Edge → Tree → Tree
addEdge = undefined

isNodeIn ∷ Node → Tree → Bool
isNodeIn = undefined

makeStep ∷ Node → Node
makeStep = undefined

randomWalk ∷ Source → Target → Steps
randomWalk = undefined

randomNode ∷ [Node] → Node
randomNode = undefined

generateTable ∷ Int → Table
generateTable = undefined

plotMaze ∷ Table → Tree → IO ()
plotMaze = undefined

main ∷ IO ()
main = do
  nn ← getLine

  let size ∷ Int
      size = read nn ∷ Int

  let table ∷ Table
      table = generateTable size

  let maze ∷ Tree
      maze = getMaze table EmptyTree

  plotMaze table maze
