module Main where

import Data.List (nub)
import qualified Data.Set as S

data Direction
  = R
  | U
  | L
  | D
  deriving (Eq, Show, Ord)

parseInput input = (size, startLoc, direction, obstacles)
  where
    grid = lines input
    n = length grid
    m = length (head grid)
    size = (n, m)
    findChars chars =
      [ (row, col)
      | row <- [0 .. n - 1]
      , col <- [0 .. m - 1]
      , ((grid !! row) !! col) `elem` chars
      ]
    obstacles = findChars ['#']
    startLoc = head $ findChars ['^', 'v', '<', '>']
    direction =
      case (grid !! fst startLoc) !! snd startLoc of
        '>' -> R
        '<' -> L
        'v' -> D
        '^' -> U
        _ -> error "Invalid direction"

turn :: Direction -> Direction
turn R = D
turn D = L
turn L = U
turn U = R

move loc dir =
  case dir of
    R -> (r, c + 1)
    L -> (r, c - 1)
    D -> (r + 1, c)
    U -> (r - 1, c)
  where
    (r, c) = loc

outOfBounds :: (Int, Int) -> (Int, Int) -> Bool
outOfBounds (n, m) (r, c) = r < 0 || r >= n || c < 0 || c >= m

isObstacle :: S.Set (Int, Int) -> (Int, Int) -> Bool
isObstacle obs loc = loc `S.member` obs

findVisited :: ((Int, Int), (Int, Int), Direction, [(Int, Int)]) -> [(Int, Int)]
findVisited ((n, m), startLoc, direction, obstacles) = visited
  where
    go :: (Int, Int) -> Direction -> [(Int, Int)] -> [(Int, Int)]
    go loc dir visited
      | nextLoc `elem` obstacles = go loc (turn dir) visited
      | outOfBounds (n, m) (nr, nc) = visited
      | otherwise = go nextLoc dir (nextLoc : visited)
      where
        nextLoc = move loc dir
        (nr, nc) = move loc dir
    visited = nub $ go startLoc direction [startLoc]

part1 ((n, m), startLoc, direction, obstacles) =
  length $ findVisited ((n, m), startLoc, direction, obstacles)

part2 ((n, m), startLoc, direction, obstacles) = numCyclical
  where
    go loc dir visited obs
      | (loc, dir) `S.member` visited = True -- If we've been in the exact same state before, it's a loop
      | outOfBounds (n, m) (r, c) = False -- Out of bounds
      | nextLoc `S.member` obs =
        go loc (turn dir) (S.insert (loc, dir) visited) obs -- Turn right if blocked
      | otherwise = go nextLoc dir (S.insert (loc, dir) visited) obs -- Move forward
      where
        (r, c) = loc
        nextLoc = move loc dir
    extraObsticles =
      filter (/= startLoc) $
      findVisited ((n, m), startLoc, direction, obstacles)
    originalObsticles = S.fromList obstacles
    numCyclical =
      sum $
      map
        (fromEnum .
         go startLoc direction S.empty . (`S.insert` originalObsticles))
        extraObsticles

main :: IO ()
main = do
  input <- readFile "data/day6.txt"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input
