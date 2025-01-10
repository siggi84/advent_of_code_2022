import Data.List.Split (splitOn)
import Data.List

data Direction = UP | DOWN | RIGHT | LEFT deriving (Show)
data Location = L { x :: Int,
                    y :: Int } deriving (Show, Eq)


parseInput :: String -> [Direction]
parseInput s = res where
  tmp = map ((\(t:n:ts) -> take (read n :: Int) (repeat (case t of
                         "U" -> UP
                         "D" -> DOWN
                         "R" -> RIGHT
                         "L" -> LEFT
                         _ -> error "Wrong data format"))) . words) (lines s)
  res = foldl (++) [] tmp


sign :: Int -> Int
sign x | x < 0 = -1
       | x > 0 = 1
       | otherwise = 0

mover :: Location -> Direction -> Location
mover (L xi yi) UP    =  L xi (yi+1)
mover (L xi yi) DOWN  =  L xi (yi-1)
mover (L xi yi) RIGHT =  L (xi+1) yi
mover (L xi yi) LEFT  =  L (xi-1) yi

part_helper :: [Direction] -> Int -> Int
part_helper ss m = res where
  head_location = scanl mover (L 0 0) ss 
  follower (L tx ty) (L hx hy) | ty == hy && abs delta_x > 1   = (L (tx + (sign delta_x)) ty)
                               | tx == hx && abs delta_y > 1   = (L  tx (ty + (sign delta_y)))
                               | abs delta_x + abs delta_y > 2 =  L (tx + (sign delta_x)) (ty + (sign delta_y))
                               | otherwise = L tx ty where
    delta_x = hx - tx
    delta_y = hy - ty
  follow_itr = iterate $ scanl follower (L 0 0) . tail
  tail_location = follow_itr head_location !! m
  res = length (nub tail_location)

part1 :: [Direction] -> Int
part1 ss = part_helper ss 1

part2 :: [Direction] -> Int
part2 ss = part_helper ss 9

main = do
  input <- readFile "input.dat"
  let parsed_input = parseInput input
  print (part1 parsed_input)
  print (part2 parsed_input)
