import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

data Coord =
  C Int Int
  deriving (Eq, Ord, Show)

addCoord (C x1 y1) (C x2 y2) = C (x1 + x2) (y1 + y2)

getX (C x _) = x

getY (C _ y) = y

type CoordCollection = Set.Set Coord

inCollection = Set.member

parseInput :: String -> CoordCollection
parseInput s = res
  where
    handleLine (li, l) = map (\(i, c) -> C li i) $ filter (\(i, c) -> c == '#') (zip [0 ..] l)
    res = Set.fromList $ concatMap handleLine (zip [0 ..] (lines s))

evolve :: (CoordCollection, Bool) -> Int -> (CoordCollection, Bool)
evolve (elveLocations, hasConverged) iteration = (result, converged)
  where
    directionCollection =
      take 4 $
      drop (mod iteration 4)  $
      cycle
        [ [C (-1) 0, C (-1) 1, C (-1) (-1)]
        , [C 1 0, C 1 1, C 1 (-1)]
        , [C 0 (-1), C 1 (-1), C (-1) (-1)]
        , [C 0 1, C 1 1, C (-1) 1]
        ]
    allDirections = nub $ concat directionCollection
    makeSuggestion e =
      if allEmpty
        then Nothing
        else res
      where
        allNeighbours = concat coordChecks
        allEmpty = not (checkDir allNeighbours)
        coordChecks = map (map (addCoord e)) directionCollection
        dchecks = dropWhile checkDir coordChecks
        checkDir = any (`inCollection` elveLocations)
        res =
          case dchecks of
            [] -> Nothing
            _ -> Just (head (head dchecks))

    allSuggestions =
      Set.toList $
      Set.map (\(s, e) -> (fromJust s, e)) $
      Set.filter (isJust . fst) $ Set.map (\e -> (makeSuggestion e, e)) elveLocations
    acceptedSuggestions =
      map head $ filter (\g -> length g == 1) $ groupBy (\s1 s2 -> fst s1 == fst s2) allSuggestions
    toRemove = Set.fromList $ map snd acceptedSuggestions
    toAdd = Set.fromList $ map fst acceptedSuggestions
    converged = hasConverged || Set.null toRemove
    result = Set.union (Set.difference elveLocations toRemove) toAdd

part1 :: CoordCollection -> Int
part1 cc = (maxX - minX + 1) * (maxY - minY + 1) - length finalState
  where
    finalState :: CoordCollection
    n = 10
    finalState = fst $ foldl evolve (cc, False) [0 .. (n - 1)]
    maxX = maximum $ Set.map getX finalState
    minX = minimum $ Set.map getX finalState
    maxY = maximum $ Set.map getY finalState
    minY = minimum $ Set.map getY finalState

part2 :: CoordCollection -> Int
part2 cc = res
  where
    finalState = scanl evolve (cc, False) [0 .. ]
    res = fromJust $ findIndex snd finalState

main = do
  inputString <- readFile "input.dat"
  let parsedInput = parseInput inputString
  print (part1 parsedInput)
  print (part2 parsedInput)
