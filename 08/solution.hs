import Data.Char
import Data.List

parseInput :: String -> [[Int]]
parseInput s = map (map digitToInt) . lines $ s

fromLeft :: [[Int]] -> [[Int]]
fromLeft d = res where
  tmp = map (scanl max (-1)) d
  res = map (reverse . tail . reverse) tmp

part1 :: [[Int]] -> Int
part1 d = res where
  left = fromLeft d
  right = map reverse (fromLeft (map reverse d))
  top = transpose (fromLeft (transpose d))
  bottom_d = map reverse (transpose d)
  bottom = transpose (map reverse (fromLeft bottom_d))

  helper1d :: [Int] -> [Int] -> [Int]
  helper1d l r = (map (\t -> min (fst t) (snd t)) (zip l r))

  helper2d :: [[Int]] -> [[Int]] -> [[Int]]
  helper2d l r = map (\t -> (helper1d (fst t) (snd t))) (zip l r)

  helper1db l d = (map (\t -> if (fst t) < (snd t) then 1 else 0) (zip l d))
  helper2db l d = map (\t -> (helper1db (fst t) (snd t))) (zip l d)

  min_height_to_edge = helper2d (helper2d left right) (helper2d top bottom)
  is_visible = helper2db min_height_to_edge d
  res = sum (map sum is_visible)

main = do
  input <- readFile "input.dat"
  let forrest = parseInput input
  -- print (fromLeft forrest)
  print (part1 forrest)
