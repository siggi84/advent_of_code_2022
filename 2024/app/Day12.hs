import Data.Array
import Data.Char (digitToInt)
import Data.Ix (inRange)
import Data.List (nub, sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

stringTo2DArray :: String -> Array (Int, Int) Char
stringTo2DArray input =
  let rows = lines input
      height = length rows
      width =
        if null rows
          then 0
          else length (head rows)
      coordsValues =
        [ ((r, c), char)
        | (r, row) <- zip [0 ..] rows
        , (c, char) <- zip [0 ..] row
        ]
   in array ((0, 0), (height - 1, width - 1)) coordsValues

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

findCluster :: (Int, Int) -> Array (Int, Int) Char -> [(Int, Int)]
findCluster start grid = go [start] []
  where
    (x, y) = start
    val = grid ! (x, y)
    go :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    go [] visited = visited
    go (v:vs) visited = go vs' cluster'
      where
        nbs = filter (inRange (bounds grid)) (neighbors v)
        validNbs =
          filter
            (\n -> grid ! n == val && n `notElem` vs && n `notElem` visited)
            nbs
        cluster' = nub $ v : visited
        vs' = nub $ vs ++ validNbs

clusterScore :: [(Int, Int)] -> Int
clusterScore cluster = area * perim
  where
    area = length cluster
    diagNeighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    nnb = concatMap diagNeighbors cluster
    perim = length $ filter (`notElem` cluster) nnb

findClusters grid = go [] ind'
  where
    ind' = indices grid
    go :: [[(Int, Int)]] -> [(Int, Int)] -> [[(Int, Int)]]
    go clusters [] = clusters
    go clusters (v:vs) = go clusters' vs'
      where
        cluster = findCluster v grid
        clusters' = cluster : clusters
        vs' = filter (`notElem` cluster) vs

-- findEdgeCluster :: Edge -> [Edge] -> [Edge]
part1 grid = sum $ map clusterScore clusters
  where
    clusters = findClusters grid

type Edge = ((Int, Int), (Int, Int))

calculateEdgeCount cluster = length $ groupEdgeClusters regionEdges []
  where
    regionEdges = concatMap edgesWithDirections cluster
    edgesWithDirections :: (Int, Int) -> [Edge]
    edgesWithDirections (x, y) = nbs
      where
        nbs = map dir $ filter (`notElem` cluster) $ neighbors (x, y)
        dir (x', y') = ((x, y), (y - y', x' - x))
    connectedEdges ((x, y), (dx, dy)) =
      filter
        (`elem` regionEdges)
        [((x + dx, y + dy), (dx, dy)), ((x - dx, y - dy), (dx, dy))]
    findConnectedEdgeGroup :: [Edge] -> [Edge] -> [Edge]
    findConnectedEdgeGroup [] cluster = cluster
    findConnectedEdgeGroup (e:es) cl = findConnectedEdgeGroup es' cl'
      where
        validNbs = filter (`notElem` cl) $ connectedEdges e
        cl' = nub $ e : cl
        es' = nub $ es ++ validNbs
    groupEdgeClusters :: [Edge] -> [[Edge]] -> [[Edge]]
    groupEdgeClusters [] clusters = clusters
    groupEdgeClusters (currentEdge:remainingEdges) clusters =
      groupEdgeClusters remainingEdges' clusters'
      where
        currentCluster = findConnectedEdgeGroup [currentEdge] []
        clusters' = currentCluster : clusters
        remainingEdges' = filter (`notElem` currentCluster) remainingEdges

part2 grid = sum $ zipWith (*) clusterSizes clusterEdges
  where
    clusters = findClusters grid
    clusterSizes = map length clusters
    clusterEdges = map calculateEdgeCount clusters

main :: IO ()
main = do
  input <- readFile "data/day12.txt"
  let grid = stringTo2DArray input
  print $ part1 grid
  print $ part2 grid
