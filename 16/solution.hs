import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

data Valve =
  Valve
    { name :: String
    , rate :: Int
    , neighbours :: [String]
    }
  deriving (Show)

parseInput :: String -> Map.Map String Valve
parseInput s = nodesSet
  where
    replace =
      map
        (\c ->
           case c of
             '=' -> ' '
             ';' -> ' '
             ',' -> ' '
             _ -> c)
    ls = map (words . replace) (lines s)
    generateNode l = Valve (l !! 1) (read (l !! 5)) (drop 10 l)
    nodes = map generateNode ls
    nodesSet = Map.fromList (map (\n -> (name n, n)) nodes)

cartProd2 xs ys = [(x, y) | x <- xs, y <- ys]

cartProd3 xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs]

allShortestPaths :: Map.Map String Valve -> Map.Map (String, String) Int
allShortestPaths g = foldl update dists (cartProd3 allNodes allNodes allNodes)
  where
    largeNumber = 10000000
    allNodes = Map.keys g
    n = length allNodes
    dists :: Map.Map (String, String) Int
    dists =
      Map.fromList $
      map
        (\(n1, n2) ->
           ( (n1, n2)
           , if n1 == n2
               then 0
               else if n1 `elem` neighbours (fromJust (Map.lookup n2 g))
                      then 1
                      else largeNumber))
        (cartProd2 allNodes allNodes)
    combs = cartProd3 allNodes allNodes allNodes
    update :: Map.Map (String, String) Int -> (String, String, String) -> Map.Map (String, String) Int
    update ds (k, i, j) =
      if ijDist > ijCand
        then Map.insert (i, j) ijCand ds
        else ds
      where
        ijDist = fromJust (Map.lookup (i, j) ds)
        ikDist = fromJust (Map.lookup (i, k) ds)
        kjDist = fromJust (Map.lookup (k, j) ds)
        ijCand = ikDist + kjDist

maxPressureRelease t g keysOfInterest allPairsShortestPath = helper "AA" t [] 0 0
    -- allPairsShortestPath = allShortestPaths g
  where
    helper :: String -> Int -> [String] -> Int -> Int -> Int
    helper v tl active cs bestSoFar
      | upperBound < bestSoFar = upperBound
      | tl < 0 = bestSoFar
      | otherwise = bestSp
      where
        valveContribution = rate (fromJust (Map.lookup v g)) * (tl - 1)
        csn = cs + valveContribution
        newBSF = max bestSoFar csn
        newActiveValves = v : active
        unactiveValves = keysOfInterest \\ newActiveValves
        upperBound =
          sum
            (map (\(vlv, vtl) -> vtl * rate (fromJust (Map.lookup vlv g))) (zip unactiveValves [tl - 3,(tl - 5) .. 0])) +
          csn
        nodeTime =
          if v == "AA"
            then 0
            else 1
        bestSp =
          foldl
            (\bsf nb ->
               max
                 bsf
                 (helper nb (tl - nodeTime - fromJust (Map.lookup (v, nb) allPairsShortestPath)) newActiveValves csn bsf))
            newBSF
            unactiveValves

rateCompare g k1 k2
  | rate (fromJust (Map.lookup k1 g)) > rate (fromJust (Map.lookup k2 g)) = LT
  | rate (fromJust (Map.lookup k1 g)) < rate (fromJust (Map.lookup k2 g)) = GT
  | otherwise = EQ

part1 g = maxPressureRelease 30 g keysOfInterest allPairsShortestPath
  where
    allPairsShortestPath = allShortestPaths g
    nodesOfInterest = Map.filterWithKey (\k v -> k == "AA" || rate v > 0) g
    keysOfInterest = sortBy (rateCompare g) (Map.keys nodesOfInterest)

part2 g = maximum cands
  where
    allPairsShortestPath = allShortestPaths g
    nodesOfInterest = Map.filterWithKey (\k v -> k == "AA" || rate v > 0) g
    keysOfInterest = sortBy (rateCompare g) (Map.keys nodesOfInterest)
    subsets :: [String] -> [[String]]
    subsets [] = [[]]
    subsets (x:xs) = subsets xs ++ map (x :) (subsets xs)
    pressureMap =
      Map.fromList $map (\l -> (l, maxPressureRelease 26 g ("AA":l) allPairsShortestPath)) (subsets keysOfInterest)
    cands =
      map
        (\l -> fromJust (Map.lookup l pressureMap) + fromJust (Map.lookup (keysOfInterest \\ l) pressureMap))
        (subsets keysOfInterest)

main = do
  inputString <- readFile "input.dat"
  let parsedInput = parseInput inputString
  print (part1 parsedInput)
  print (part2 parsedInput)
