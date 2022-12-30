import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace

data Direction
  = RIGHT
  | LEFT
  | DOWN
  deriving (Show)

type Coord = (Int, Int)

type Shape = [Coord]

parseInput :: String -> [Direction]
parseInput s = map parseChar (head $ lines s)
  where
    parseChar c
      | c == '>' = RIGHT
      | c == '<' = LEFT
      | otherwise = error "Unexpected input"

getShape i h = map (\(x, y) -> (2 + x, y + h)) (shapes !! mod i n)
  where
    shapes =
      [ [(0, 0), (1, 0), (2, 0), (3, 0)]
      , [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
      , [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
      , [(0, 0), (0, 1), (0, 2), (0, 3)]
      , [(0, 0), (0, 1), (1, 0), (1, 1)]
      ]
    n = length shapes

type State = (Shape, Int, Shape, Int)

part12 goalNumRocks ds = finalHeight
  where
    width = 7
    gridHistorySize = 120
    move :: Shape -> Direction -> Shape
    move rock LEFT = map (\(x, y) -> (x - 1, y)) rock
    move rock RIGHT = map (\(x, y) -> (x + 1, y)) rock
    move rock DOWN = map (\(x, y) -> (x, y - 1)) rock
    collidesWithWall :: Shape -> Bool
    collidesWithWall = any (\(x, y) -> x < 0 || x >= width)
    collidesWithGrid grid = any (`elem` grid)
    collides grid rock = collidesWithWall rock || collidesWithGrid grid rock
    floor = map (\x -> (x, -1)) [0 .. width - 1]
    maxGridHeight grid = maximum (map snd grid) + 1
    evolve :: State -> (Int, Direction) -> State
    evolve state (directionId, direction) = (updatedGrid, updatedRockId, updatedRock, directionId)
      where
        (grid, rockId, currentRock, _) = state
        rockCand = move currentRock direction
        rockCandCol = collides grid rockCand
        movedRock =
          if rockCandCol
            then currentRock
            else rockCand
        downRock = move movedRock DOWN
        rockDownCol = collides grid downRock
        updatedGrid =
          if rockDownCol
            then take gridHistorySize (movedRock ++ grid)
            else grid
        updatedRockId =
          if rockDownCol
            then rockId + 1
            else rockId
        updatedGridHeight = maxGridHeight updatedGrid
        updatedRock =
          if rockDownCol
            then getShape updatedRockId (updatedGridHeight + 3)
            else downRock
  -- This function identifies cycles and uses it to calculate the number of rocks and 
  -- height if the cycle would be repeated as often as possible to get to the target.
    historyWarp (stateMap, heightDelta, rockIdDelta, _, _) state
      | heightDelta > 0 = (stateMap, heightDelta, rockIdDelta, updatedHeight, updatedRockId)
      | keyInMap = (stateMap, num_rep * (height - oldHeight), num_rep * (rockId - oldRockId), height, rockId)
      | otherwise = (updatedStateMap, 0, 0, height, rockId)
      where
        (grid, rockId, currentRock, directionId) = state
        height = maxGridHeight grid
        updatedHeight = height + heightDelta
        updatedRockId = rockId + rockIdDelta
        adjustedGrid = map (\(x, y) -> (x, y - height)) grid
        adjustedRock = map (\(x, y) -> (x, y - height)) currentRock
        updatedStateMap = Map.insert key value stateMap
        key = (adjustedGrid, mod rockId 5, adjustedRock)
        value = (height, rockId)
        keyInMap = Map.member key stateMap
        (oldHeight, oldRockId) = fromJust (Map.lookup key stateMap)
        num_rep = (goalNumRocks - rockId) `div` (rockId - oldRockId)
    history = scanl evolve (floor, 0, getShape 0 3, 0) (cycle (zip [0 ..] ds))
    warpedHistory = scanl historyWarp (Map.empty, 0, 0, 0, 0) history
    (_, _, _, finalHeight, _) = head $ filter (\(_, _, _, _, n) -> n >= goalNumRocks) warpedHistory

main = do
  inputString <- readFile "input.dat"
  let parsedInput = parseInput inputString
  print (part12 2022 parsedInput)
  print (part12 1000000000000 parsedInput)
