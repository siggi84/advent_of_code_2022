import Data.List (minimumBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.PSQueue as PQ
import qualified Data.Set as S

type Node = (Int, Int, Direction)

type Path = [Node]

type Weight = Int

type Graph = Map.Map Node [(Node, Weight)]

data Direction
  = N
  | E
  | S
  | W
  deriving (Eq, Ord, Show, Enum, Bounded)

rotationCost :: Direction -> Direction -> Int
rotationCost d1 d2 = 1000 * min clockwiseSteps counterclockwiseSteps
  where
    clockwiseSteps = (fromEnum d2 - fromEnum d1) `mod` 4
    counterclockwiseSteps = (fromEnum d1 - fromEnum d2) `mod` 4

nodeStep :: Node -> Node
nodeStep (x, y, N) = (x - 1, y, N)
nodeStep (x, y, E) = (x, y + 1, E)
nodeStep (x, y, S) = (x + 1, y, S)
nodeStep (x, y, W) = (x, y - 1, W)

parseInput s = (graph, startNode, endNodes)
  where
    locations c =
      [ (lineIndex, charIndex)
      | (lineIndex, line) <- zip [0 ..] (lines s)
      , (charIndex, c') <- zip [0 ..] line
      , c' == c
      ]
    empty = locations '.'
    [start] = locations 'S'
    [end] = locations 'E'
    nodeLocations = start : end : empty
    startNode = (fst start, snd start, E)
    endNodes = [(fst end, snd end, d) | d <- [N, E, S, W]]
    nodes = [(x, y, d) | (x, y) <- nodeLocations, d <- [N, E, S, W]]
    findNeighbours (x, y, d) = rotationNb ++ stepNbs
      where
        rotationNb =
          [ ((x, y, d1), cost)
          | d1 <- [N, E, S, W]
          , let cost = rotationCost d d1
          , d /= d1
          ]
        stepNbs =
          let n = nodeStep (x, y, d)
           in ([(n, 1) | n `elem` nodes])
    graph = Map.fromList [(n, findNeighbours n) | n <- nodes]

dijkstraAllPaths :: Graph -> Node -> Node -> Maybe ([Path], Weight)
dijkstraAllPaths graph start end = go initialQueue initialDistances initialPaths
  where
    initialQueue = PQ.singleton start 0
    initialDistances = Map.singleton start 0
    initialPaths = Map.singleton start [[start]]
    go queue distances paths
      | PQ.null queue = Nothing
      | current == end =
        Just (Map.findWithDefault [] current paths, currentDist)
      | otherwise = go updatedQueue updatedDistances updatedPaths
      where
        Just (current PQ.:-> currentDist, restQueue) = PQ.minView queue
        neighbors = fromMaybe [] (Map.lookup current graph)
        (updatedDistances, updatedPaths, newQueueEntries) =
          foldr update (distances, paths, []) neighbors
          where
            update (neighbor, weight) (dists, ps, entries) =
              let newDist = currentDist + weight
                  oldDist = Map.lookup neighbor dists
                  currentPaths = Map.findWithDefault [] current ps
               in case oldDist of
                    Nothing ->
                      ( Map.insert neighbor newDist dists
                      , Map.insert
                          neighbor
                          (map (++ [neighbor]) currentPaths)
                          ps
                      , (neighbor, newDist) : entries)
                    Just dist
                      | newDist < dist ->
                        ( Map.insert neighbor newDist dists
                        , Map.insert
                            neighbor
                            (map (++ [neighbor]) currentPaths)
                            ps
                        , (neighbor, newDist) : entries)
                      | newDist == dist ->
                        ( dists
                        , Map.adjust
                            (++ map (++ [neighbor]) currentPaths)
                            neighbor
                            ps
                        , entries)
                      | otherwise -> (dists, ps, entries)
        updatedQueue = foldr (uncurry PQ.insert) restQueue newQueueEntries

part1 (graph, startNode, endNodes) =
  minimum $ mapMaybe (fmap snd . dijkstraAllPaths graph startNode) endNodes

part2 (graph, startNode, endNodes) = length visitedLocs
  where
    solutions = mapMaybe (dijkstraAllPaths graph startNode) endNodes
    minDist = minimum $ map snd solutions
    paths = concatMap fst $ filter ((== minDist) . snd) solutions
    visitedLocs = S.fromList [(x, y) | (x, y, _) <- concat paths]

main :: IO ()
main = do
  input <- readFile "data/day16.txt"
  let parsedInput = parseInput input
  print $ part1 parsedInput
  print $ part2 parsedInput
