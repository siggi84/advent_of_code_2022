import Data.Array
import Data.Char (digitToInt)
import Data.Ix (inRange)
import Data.List (nub)

stringTo2DArray :: String -> Array (Int, Int) Int
stringTo2DArray input =
  let rows = lines input
      height = length rows
      width =
        if null rows
          then 0
          else length (head rows)
      coordsValues =
        [ ((r, c), digitToInt char)
        | (r, row) <- zip [0 ..] rows
        , (c, char) <- zip [0 ..] row
        ]
   in array ((0, 0), (height - 1, width - 1)) coordsValues

findIndicesWithValue :: Eq a => a -> Array (Int, Int) a -> [(Int, Int)]
findIndicesWithValue value arr = [idx | (idx, v) <- assocs arr, v == value]

neighbors :: (Int, Int) -> Array (Int, Int) Int -> [(Int, Int)]
neighbors (x, y) grid = filter (\l -> (grid ! l) == vVal + 1) nbValid  
  where
    vVal = grid ! (x, y)
    gridBounds = bounds grid
    nbCandidates = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    nbValid = filter (inRange gridBounds) nbCandidates
    

part1 :: Array (Int, Int) Int -> Int
part1 grid = sum $ map (length . helper) trailheads
  where
    trailheads = findIndicesWithValue 0 grid
    helper v =
      if (grid ! v) == 9
        then [v]
        else nub $ concatMap helper $ neighbors v grid

part2 :: Array (Int, Int) Int -> Int
part2 grid = sum $ map helper trailheads
  where
    trailheads = findIndicesWithValue 0 grid
    helper v =
      if (grid ! v == 9)
        then 1
        else sum $ map helper $ neighbors v grid

main :: IO ()
main = do
  input <- readFile "data/day10.txt"
  let grid = stringTo2DArray input
  print $ part1 grid
  print $ part2 grid
