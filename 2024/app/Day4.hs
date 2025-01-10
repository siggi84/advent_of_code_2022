import Data.Maybe (Maybe)
import Data.Maybe (catMaybes)

directions :: [(Int, Int)]
directions =
  [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]

safeAccess :: [[a]] -> (Int, Int) -> Maybe a
safeAccess grid (row, col)
  | row < 0 || row >= length grid = Nothing
  | col < 0 || col >= length (grid !! row) = Nothing
  | otherwise = Just $ grid !! row !! col

getString :: [[a]] -> (Int, Int) -> Int -> Int -> Int -> [a]
getString grid (dRow, dCol) row col len =
  catMaybes $ take len $ map (safeAccess grid) coords
  where
    coords = [(row + i * dRow, col + i * dCol) | i <- [0 ..]]

getAllStrings :: [[a]] -> Int -> Int -> Int -> [[a]]
getAllStrings grid row col len =
  [getString grid dir row col len | dir <- directions]

part1 :: [[Char]] -> Int
part1 grid =
  length . filter (== "XMAS") . concat $
  [getAllStrings grid row col 4 | row <- [0 .. n - 1], col <- [0 .. m - 1]]
  where
    n = length grid
    m = length (head grid)

xmasDetector grid x y =
  safeAccess grid (x + 1, y + 1) == Just 'A' &&
  isMasPair (safeAccess grid (x, y)) (safeAccess grid (x + 2, y + 2)) &&
  isMasPair (safeAccess grid (x + 2, y)) (safeAccess grid (x, y + 2))
  where
    isMasPair a b =
      (a, b) == (Just 'M', Just 'S') || (a, b) == (Just 'S', Just 'M')

part2 :: [[Char]] -> Int
part2 grid = length $ filter id matches
  where
    gridHeight = length grid
    gridWidth = length (head grid)
    matches =
      [ xmasDetector grid x y
      | x <- [0 .. gridHeight - 1]
      , y <- [0 .. gridWidth - 1]
      ]

main :: IO ()
main = do
  grid <- lines <$> readFile "data/day4.txt"
  print (part1 grid, part2 grid)
