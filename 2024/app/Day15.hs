import Data.Bifunctor (second)
import Data.List (nub, sort)
import Data.List.Split (splitOn)
import qualified Data.Set as S

data Warehouse =
  Warehouse
    { walls :: S.Set (Int, Int)
    , boxes :: S.Set (Int, Int)
    , robot :: (Int, Int)
    }
  deriving (Show, Eq)

data Moves
  = UP
  | DOWN
  | LEFT
  | RIGHT
  deriving (Show, Eq)

parseMove c
  | c == '^' = UP
  | c == '>' = RIGHT
  | c == '<' = LEFT
  | c == 'v' = DOWN
  | otherwise = error "Unknown symbol"

move UP (x, y) = (x - 1, y)
move DOWN (x, y) = (x + 1, y)
move LEFT (x, y) = (x, y - 1)
move RIGHT (x, y) = (x, y + 1)

parseInput s = (Warehouse wallLocations boxLocations robotLocation, moves)
  where
    splitInput = splitOn "\n\n" s
    gridString = head splitInput
    locations c =
      [ (lineIndex, charIndex)
      | (lineIndex, line) <- zip [0 ..] (lines gridString)
      , (charIndex, c') <- zip [0 ..] line
      , c' == c
      ]
    moves = map parseMove $ filter (/= '\n') $ head $ tail splitInput
    wallLocations = S.fromList $ locations '#'
    boxLocations = S.fromList $ locations 'O'
    robotLocation = head $ locations '@'

evolve warehouse m
  | moveToWall = warehouse
  | otherwise = warehouse {robot = robot', boxes = updatedBoxes}
  where
    robot' = move m (robot warehouse)
    bxs =
      takeWhile (\l -> l `S.member` boxes warehouse) (iterate (move m) robot')
    end = move m $ last (robot warehouse : bxs)
    moveToWall = end `S.member` walls warehouse
    movedBoxes = S.fromList $ map (move m) bxs
    updatedBoxes =
      S.union movedBoxes $ boxes warehouse `S.difference` S.fromList bxs

part1 warehouse moves = score
  where
    evolvedWarehouse = foldl evolve warehouse moves
    score =
      sum $ map (\(x, y) -> 100 * x + y) (S.toList (boxes evolvedWarehouse))

evolveWide warehouse m
  | robotMoveToWall || boxMoveToWall = warehouse
  | otherwise = warehouse'
  where
    robot' = move m (robot warehouse)
    bxs = affectedBoxes robot' m
    updatedBxs = S.fromList $ map (move m) bxs
    rightBxs = S.map (\(x, y) -> (x, y + 1)) updatedBxs
    robotMoveToWall = robot' `S.member` walls warehouse
    boxMoveToWall =
      any (`S.member` walls warehouse) (S.union updatedBxs rightBxs)
    updatedBoxes =
      S.union updatedBxs $ boxes warehouse `S.difference` S.fromList bxs
    warehouse' = warehouse {robot = robot', boxes = updatedBoxes}
    end = move m $ last (robot warehouse : bxs)

    affectedBoxesLR current mv
      | hitsBox = loc : affectedBoxes mvd mv
      | otherwise = []
      where
        loc =
          case mv of
            LEFT -> move LEFT current
            RIGHT -> current
        mvd = move mv $ move mv current
        hitsBox = loc `S.member` boxes warehouse

    affectedBoxes current LEFT = affectedBoxesLR current LEFT
    affectedBoxes current RIGHT = affectedBoxesLR current RIGHT
    affectedBoxes current mv
      | hitsBox = nub (rCascade ++ lCascade)
      | otherwise = []
      where
        hitsBoxR = current `S.member` boxes warehouse
        hitsBoxL = move LEFT current `S.member` boxes warehouse
        hitsBox = hitsBoxR || hitsBoxL
        rCascade
          | hitsBoxR =
            current :
            affectedBoxes (move mv current) mv ++
            affectedBoxes (move RIGHT $ move mv current) mv
          | otherwise = []
        lCascade
          | hitsBoxL =
            move LEFT current :
            affectedBoxes (move mv current) mv ++
            affectedBoxes (move LEFT $ move mv current) mv
          | otherwise = []

part2 warehouse moves = score
  where
    updatedWalls =
      S.fromList $
      concatMap
        (\(x, y) -> [(x, 2 * y), (x, 2 * y + 1)])
        (S.toList (walls warehouse))
    updatedBoxes = S.map (second (2 *)) (boxes warehouse)
    (x, y) = robot warehouse
    updatedRobot = second (* 2) (robot warehouse)
    wideWarehouse = Warehouse updatedWalls updatedBoxes updatedRobot
    evolvedWarehouse = foldl evolveWide wideWarehouse moves
    score =
      sum $ map (\(x, y) -> 100 * x + y) (S.toList (boxes evolvedWarehouse))

main :: IO ()
main = do
  input <- readFile "data/day15.txt"
  let (warehouse, moves) = parseInput input
  print $ part1 warehouse moves
  print $ part2 warehouse moves
