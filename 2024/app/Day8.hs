import Data.Char (isSpace)
import Data.List (nub, sort)
import Data.Map (Map)
import qualified Data.Map as Map

-- Parse the grid into a Map
parseGrid :: String -> Map Char [(Int, Int)]
parseGrid str = foldl insertCoordinate Map.empty indexedSymbols
    -- Generate (row, col, symbol) triples
  where
    indexedSymbols =
      [ (row, col, c)
      | (row, line) <- zip [0 ..] (lines str)
      , (col, c) <- zip [0 ..] line
      , c /= '.'
      ]
    insertCoordinate acc (row, col, c) = Map.insertWith (++) c [(row, col)] acc

antinodes (r1, c1) (r2, c2) (n, m) = pos
  where
    dr = r2 - r1
    dc = c2 - c1
    pos =
      filter
        (not . outOfBounds (n, m))
        [(r1 - dr, c1 - dc), (r2 + dr, c2 + dc)]

antinodes2 (r1, c1) (r2, c2) (n, m) = nub $ pos1 ++ pos2
  where
    dr = r2 - r1
    dc = c2 - c1
    pos1 =
      takeWhile
        (not . outOfBounds (n, m))
        [(r1 - i * dr, c1 - i * dc) | i <- [0 ..]]
    pos2 =
      takeWhile
        (not . outOfBounds (n, m))
        [(r2 + i * dr, c2 + i * dc) | i <- [1 ..]]

outOfBounds :: (Int, Int) -> (Int, Int) -> Bool
outOfBounds (n, m) (r, c) = r < 0 || r >= n || c < 0 || c >= m

findAntinodes n m grid antiFun =
  nub $
  concat $
  [ antiFun a1 a2 (n, m)
  | (c, coords) <- Map.toList grid
  , a1 <- coords
  , a2 <- coords
  , a1 /= a2
  ]

part1 n m grid = length $ findAntinodes n m grid antinodes
part2 n m grid = length $ findAntinodes n m grid antinodes2

main :: IO ()
main = do
  input <- readFile "data/day8.txt"
  let n = length (lines input)
  let m = length (head $ lines input)
  let grid = parseGrid input
  print $ part1 n m grid
  print $ part2 n m grid
