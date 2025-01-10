import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

data Material
  = ORE
  | CLAY
  | OBSIDIAN
  | GEODE
  deriving (Ord, Eq, Show)

type RobotCost = Map.Map Material Int

type BluePrint = Map.Map Material RobotCost

materialTypes = [ORE, CLAY, OBSIDIAN, GEODE]

emptyRobotCost = Map.fromList (map (\m -> (m, 0)) materialTypes)

getBP :: Material -> BluePrint -> RobotCost
getBP m pb = fromJust (Map.lookup m pb)

get :: Material -> RobotCost -> Int
get m rc = fromJust (Map.lookup m rc)

insert :: Material -> Int -> RobotCost -> RobotCost
insert = Map.insert

parseInput :: String -> [BluePrint]
parseInput s = map handleLine (lines s)
  where
    costTemplate = emptyRobotCost
    handleLine s = Map.fromList [(ORE, oreCost), (CLAY, clayCost), (OBSIDIAN, obsidianCost), (GEODE, geodeCost)]
      where
        sl = words s
        oreCost = insert ORE (read (sl !! 6) :: Int) emptyRobotCost
        clayCost = insert ORE (read (sl !! 12) :: Int) emptyRobotCost
        obsidianCost = insert CLAY (read (sl !! 21) :: Int) (insert ORE (read (sl !! 18) :: Int) emptyRobotCost)
        geodeCost = insert OBSIDIAN (read (sl !! 30) :: Int) (insert ORE (read (sl !! 27) :: Int) emptyRobotCost)

blueprintEvaluator totalTime bp = helper totalTime startRobots startResources Nothing 0
  where
    startRobots = insert ORE 1 emptyRobotCost
    startResources = emptyRobotCost
    maxNeeded GEODE = 1000000
    maxNeeded material = maximum $ map (\r -> get material (getBP r bp)) materialTypes
    maxNumRobots :: RobotCost
    maxNumRobots = Map.fromList (zip materialTypes (map maxNeeded materialTypes))

    helper :: Int -> RobotCost -> RobotCost -> Maybe Material -> Int -> Int
    helper 0 _ numResources _ _ = get GEODE numResources
    helper timeLeft numRobots numResources Nothing bestSoFar =
      foldl (\bsf m -> max bsf (helper timeLeft numRobots numResources (Just m) bsf)) bestSoFar relevantRobots
      where
        relevantRobots = filter (\m -> get m maxNumRobots > get m numRobots) materialTypes

    helper timeLeft numRobots numResources (Just buildNext) bestSoFar
      | upperBound <= bestSoFar = bestSoFar
      | otherwise = helper (timeLeft - 1) updatedNumRobots updatedNumResources updatedBuildNext bestSoFar
      where
        upperBoundG = get GEODE numResources + timeLeft * get GEODE numRobots + (timeLeft * (timeLeft - 1) `div` 2)
        upperBoundNG = get GEODE numResources + (timeLeft-1) * get GEODE numRobots + ((timeLeft-1) * (timeLeft - 2)) `div` 2
        upperBound = if buildNext == GEODE then upperBoundG else upperBoundNG
        canAffordRobot = all (\m -> get m numResources >= get m (getBP buildNext bp)) materialTypes
        updatedNumRobots =
          if canAffordRobot
            then insert buildNext (get buildNext numRobots + 1) numRobots
            else numRobots
        buyCost =
          if canAffordRobot
            then getBP buildNext bp
            else emptyRobotCost
        updatedNumResources = Map.mapWithKey (\k v -> v + get k numRobots - get k buyCost) numResources
        updatedBuildNext = if canAffordRobot then Nothing else Just buildNext

part1 bps = sum $ map (\(index, bp) -> index * blueprintEvaluator 24 bp) (zip [1 ..] bps)

part2 bps = product $ map (blueprintEvaluator 32) (take 3 bps)

main = do
  inputString <- readFile "input.dat"
  let parsedInput = parseInput inputString
  print (part1 parsedInput)
  print (part2 parsedInput)
