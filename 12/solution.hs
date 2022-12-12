import Data.Char
import qualified Data.Map as Map
import Data.List

type Coord = (Int, Int)
type Grid = Map.Map Coord Int

parseInput :: String -> (Grid, Coord, Coord)
parseInput s = (mapGrid, start, end) where
  charGrid = words s
  n = length charGrid
  m = length (charGrid !! 0)

  charFinder c i (g:gs) = case (elemIndex c g) of
                        Just n -> (i, n)
                        _ -> charFinder c (i+1) gs
  start = charFinder 'S' 0 charGrid
  end = charFinder 'E' 0 charGrid

  converter c | c == 'S' = 0
              | c == 'E' = (ord 'z') - (ord 'a')
              | otherwise = (ord c) - (ord 'a') 
  flattened = map converter $ foldl (++) "" charGrid
  gridCoords = map (\i -> (i `div` m, mod i m)) [0 .. n*m]
  mapGrid = Map.fromList (zip gridCoords flattened)

-- I inverted the search to search from stop to start.
-- This simplifies part2.
partn :: (Grid, Coord, Coord) -> Bool -> Int
partn (g, start, stop) isPart1 = res where
  bfs :: [Coord] -> [Coord] -> Int -> Int
  bfs current visited l = res where
    neighbors :: Coord -> [Coord]
    neighbors (i, j) = filter (\c -> case ((Map.lookup (i,j) g), (Map.lookup c g)) of 
      (Just sv, Just dv) -> (sv - dv < 2)
      _                  -> False
      )[(i-1,j), (i+1,j), (i,j-1), (i,j+1)]
    
    enqued :: [Coord]
    enqued = filter (\c -> not (elem c visited)) $ nub $ concat $ map neighbors current

    condition1 = elem start current
    condition2 = any (\c -> case (Map.lookup c g) of
        (Just dv) -> dv == 0
        _ -> False) current
    condition = if isPart1 then condition1 else condition2
    res = if condition then l else (bfs enqued (visited ++ current) (l+1))
      
  -- bfs n visited path = 
  res = bfs [stop] [stop] 0


main = do
  input <- readFile "input.dat"
  let parsedInput = parseInput input
  print (partn parsedInput True)
  print (partn parsedInput False)
