import Data.List.Split (splitOn)
import qualified Data.Map as Map

type Coord = (Int, Int)

type Grid = Map.Map Coord Char

parseInput s = rockGrid
  where
    rockGrid = Map.fromList (zip rockLines (repeat '#'))
    handleLine :: String -> [Coord]
    handleLine l = createCoords $ map ((\[e1, e2] -> (read e1 :: Int, read e2 :: Int)) . splitOn ",") $ splitOn "->" l
    rockLines = concatMap handleLine $ lines s
    createCoords :: [Coord] -> [Coord]
    createCoords [] = []
    createCoords [c1] = [c1]
    createCoords (c1:c2:cs) = rockGrid ++ createCoords (c2 : cs)
      where
        (x1, y1) = c1
        (x2, y2) = c2
        delta_x = x2 - x1
        delta_y = y2 - y1
        n = max (abs delta_x) (abs delta_y)
        dx = delta_x `div` n
        dy = delta_y `div` n
        rockGrid = map (\i -> (x1 + i * dx, y1 + i * dy)) [0 .. n - 1]

part1 :: Grid -> Int
part1 grid = solver grid True

part2 :: Grid -> Int
part2 grid = solver grid False + 1

solver grid part1 = helper grid 0
  where
    startCoord = (500, 0)
    part2 = not part1
    maxRock = maximum (map snd (Map.keys grid))
    floor = maxRock + 2
    findLanding :: Coord -> Grid -> (Grid, Bool)
    findLanding (x, y) g
      | part1 && y > maxRock           = (g, True)
      | part2 && (y + 1) == floor      = (Map.insert (x, y) 'o' g, False)
      | Map.notMember (x, y + 1) g     = findLanding (x, y + 1) g
      | Map.notMember (x - 1, y + 1) g = findLanding (x - 1, y + 1) g
      | Map.notMember (x + 1, y + 1) g = findLanding (x + 1, y + 1) g
      | part2 && (x, y) == startCoord  = (g, True)
      | otherwise                      = (Map.insert (x, y) 'o' g, False)

    helper :: Grid -> Int -> Int
    helper g i = if b then i else helper gn (i + 1)
      where
        (gn, b) = findLanding startCoord g

main = do
  input <- readFile "input.dat"
  let parsedInput = parseInput input
  print (part1 parsedInput)
  print (part2 parsedInput)
