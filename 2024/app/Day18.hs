{-# LANGUAGE TupleSections #-}

import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import qualified Data.PSQueue as PQ

type Node = (Int, Int)

type Path = [Node]

type Weight = Int

type Graph = Map.Map Node [(Node, Weight)]

parseInput :: String -> [(Int, Int)]
parseInput s = map ((\(x1:x2:xr) -> (read x1, read x2)) . splitOn ",") $ lines s

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

createGraph n coords = (graph, start, end)
  where
    nodes =
      [c | x <- [0 .. n], y <- [0 .. n], let c = (x, y), c `notElem` coords]
    start = (0, 0)
    end = (n, n)
    neighbors (x', y') =
      filter
        (\(x, y) ->
           (x >= 0 && x <= n && y >= 0 && y <= n) && (x, y) `notElem` coords)
        [(x' + 1, y'), (x' - 1, y'), (x', y' - 1), (x', y' + 1)]
    graph = Map.fromList [(c, map ((, 1)) (neighbors c)) | c <- nodes]

part1 n coords = fmap snd res
  where
    (graph, start, end) = createGraph n coords
    res = dijkstraAllPaths graph start end

binarySearch :: Int -> Int -> (Int -> Bool) -> Int
binarySearch lo hi p
  | lo == hi = lo
  | p mid = binarySearch lo mid p
  | otherwise = binarySearch (mid + 1) hi p
  where
    mid = (lo + hi) `div` 2

part2 n coords = last (take (binarySearch 0 numCoords go) coords)
  where
    numCoords = length coords
    go mid = not hasPath
      where
        coords' = take mid coords
        (graph, start, end) = createGraph n coords'
        res = dijkstraAllPaths graph start end
        hasPath = isJust res

main :: IO ()
main = do
  testInput <- readFile "data/day18_test.txt"
  let parsedTestInput = parseInput testInput
  input <- readFile "data/day18.txt"
  let parsedInput = parseInput input
  print $ part1 6 (take 12 parsedTestInput)
  print $ part1 70 (take 1024 parsedInput)
  print $ part2 6 parsedTestInput
  print $ part2 70 parsedInput
  print $ part1 70 (take 2957 parsedInput)
