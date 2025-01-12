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

part2 :: [[Int]] -> Int
part2 d = res where
  right_helper :: [Int] -> Int
  right_helper [] = error "Error"
  right_helper (v:[]) = 0
  right_helper (v:vs) = res where 
    tmp = (length (takeWhile (\t -> (t < v)) vs))
    vs_len = length vs
    res = if tmp == vs_len then tmp else tmp+1

  right_helper2 :: [Int] -> [Int]
  right_helper2 (v:[]) = [right_helper [v]]
  right_helper2 v = (right_helper v):(right_helper2 (tail v))

  multi_right_helper = map right_helper2

  left = multi_right_helper d
  right = map reverse (multi_right_helper (map reverse d))
  top = transpose (multi_right_helper (transpose d))
  bottom_d = map reverse (transpose d)
  bottom = transpose (map reverse (multi_right_helper bottom_d))

  listMul (x:xs) (y:ys) = x * y : (listMul xs ys)
  listMul [] [] = []
  listMul _ _ = error "Lengths do not match"
  listMul2d x y = map (\t -> listMul (fst t) (snd t)) (zip x y)

  tmp = listMul2d (listMul2d left right) (listMul2d top bottom)
  res = maximum $ map maximum tmp

main = do
  input <- readFile "input.dat"
  let forrest = parseInput input
  print (part1 forrest)
  print (part2 forrest)
