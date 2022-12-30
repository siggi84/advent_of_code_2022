import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace

type Coord = (Int, Int, Int)

getX (x, _, _) = x

getY (_, y, _) = y

getZ (_, _, z) = z

parseInput :: String -> Set.Set Coord
parseInput s = Set.fromList $ map ((\sl -> (read (sl !! 0), read (sl !! 1), read (sl !! 2))) . splitOn ",") (lines s)

neighbours (x, y, z) = [(x - 1, y, z), (x + 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1)]

part1 pl = length neighboursInSet
  where
    neighboursInSet = filter (`Set.notMember` pl) $ concat $ Set.map neighbours pl

cartProd3 xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs]

part2 :: Set.Set Coord -> Int
part2 pl = length neighboursInSet
  where
    min_x = minimum (Set.map getX pl) - 1
    min_y = minimum (Set.map getY pl) - 1
    min_z = minimum (Set.map getZ pl) - 1
    max_x = maximum (Set.map getX pl) + 1
    max_y = maximum (Set.map getY pl) + 1
    max_z = maximum (Set.map getZ pl) + 1

    inBox (x, y, z) = min_x <= x && x <= max_x && min_y <= y && y <= max_y && min_z <= z && z <= max_z
    helper :: Set.Set Coord -> Set.Set Coord -> Set.Set Coord
    helper airSet front
      | null front = airSet
      | otherwise = helper updatedAirSet newFront
      where
        updatedAirSet = Set.union airSet front
        newFront =
          Set.filter (\c -> Set.notMember c airSet && Set.notMember c pl && inBox c) $
          Set.fromList $ concat $ Set.map neighbours front
    airSet = helper Set.empty (Set.fromList [(min_x, min_y, min_z)])
    neighboursInSet = filter (\c -> Set.notMember c pl && Set.member c airSet) $ concat $ Set.map neighbours pl

main = do
  inputString <- readFile "input.dat"
  let parsedInput = parseInput inputString
  print (part1 parsedInput)
  print (part2 parsedInput)
