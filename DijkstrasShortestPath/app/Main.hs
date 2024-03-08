import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Data.Set (Set)
import qualified Data.Set as S

type Node = Char
type Weight = Int

main :: IO ()
main = do
  putStrLn "Enter the start node: "
  startNode <- getLine
  let start = head startNode

  putStrLn "Enter the end node: "
  endNode <- getLine
  let end = head endNode

  -- Dijkstra's kortstepad-algoritme uitvoeren
  let (minDistances, previousNodes) = shortestPath start connectedNodes
      path = shortestPathTo end previousNodes

  putStrLn $ "Distance from " ++ [start] ++ " to " ++ [end] ++ " = " ++ show (minDistances ! end)
  putStrLn $ "Path: " ++ show path

-- De gewogen graaf, hetzelfde opgesteld als in de demo
connectedNodes :: Array Node [(Node, Weight)]
-- Onze lijst bestaat uit maar 8 nodes, a tot h
connectedNodes = listArray ('a', 'h') [
  [('d',4), ('c',6)], -- A
  [('g',7), ('h',2)], -- B
  [('a',6), ('d',3), ('e',2), ('g',12), ('f',7)], -- C
  [('a',4), ('c',3), ('e',7)], -- D
  [('c',2), ('d',7), ('g',9)], -- E
  [('c',7), ('h',10)], -- F
  [('b',7), ('c',12), ('e',9), ('h',4)], -- G
  [('b',2), ('f',10), ('g',4)] -- H
  ]

shortestPath :: Node -> Array Node [(Node, Weight)] -> (Array Node Weight, Array Node Node)
shortestPath startNode connectedNodes = runST $ do
  -- Bepaal de grenzen van de array
  let boundsArray = bounds connectedNodes
  -- Maak een array aan met oneindige afstanden
  minDistance <- newArray boundsArray maxBound
  -- Zet de afstand tot startnode op 0
  writeArray minDistance startNode 0
  previous <- newArray boundsArray ' '
  -- Verken de buren
  exploreNeighbors (S.singleton (0, startNode)) minDistance previous
  (,) <$> freeze minDistance <*> freeze previous

exploreNeighbors :: Set (Weight, Node) -> STArray s Node Weight -> STArray s Node Node -> ST s ()
exploreNeighbors nodeQueue minDistance previous =
  case S.minView nodeQueue of
    -- Als de queue leeg is, stop
    Nothing -> return ()
    Just ((distance, neighborNode), nodeQueue') -> do
      -- Haal de verbonden nodes (buren) op
      let edges = connectedNodes ! neighborNode
      -- Verken de buren
      exploreEdges edges nodeQueue' distance neighborNode minDistance previous

exploreEdges :: [(Node, Weight)] -> Set (Weight, Node) -> Weight -> Node -> STArray s Node Weight -> STArray s Node Node -> ST s ()
exploreEdges [] nodeQueue _ _ minDistance previous = exploreNeighbors nodeQueue minDistance previous
-- Als er geen buuren zijn, ga verder met de volgende node
exploreEdges ((currentNode, weight):edges) nodeQueue distance neighborNode minDistance previous = do
  oldDistance <- readArray minDistance currentNode
  -- Bereken de nieuwe afstand
  let distanceThroughNeighbor = distance + weight
  -- Als we een kortere afstand hebben gevonden
  if distanceThroughNeighbor < oldDistance
    then do
      -- Update de afstand tot de buur node
      writeArray minDistance currentNode distanceThroughNeighbor
      -- Update de vorige node
      writeArray previous currentNode neighborNode
      -- Update de queue
      let updatedQueue = S.insert (distanceThroughNeighbor, currentNode) $ S.delete (oldDistance, currentNode) nodeQueue
      -- Ga verder met de rest van de randen
      exploreEdges edges updatedQueue distance neighborNode minDistance previous
    else
      exploreEdges edges nodeQueue distance neighborNode minDistance previous


shortestPathTo :: Node -> Array Node Node -> [Node]
shortestPathTo target previous =
  exploreNeighbors target []
  where
    -- Ga recursief alle nodes na
    exploreNeighbors node path
      -- Als we een lege node tegenkomen, stop
      | node == ' ' = path
      -- Anders voeg de huidige node toe aan de lijst en ga verder met de vorige node
      | otherwise = exploreNeighbors (previous ! node) (node : path)
