import Data.List (intersect)
import Data.List.Split (splitOn)
import Data.Char (ord)

main :: IO()

charPriority :: Char -> Int
charPriority c
  | c >= 'a'  = (ord c) - (ord 'a') + 1 
  | otherwise = (ord c) - (ord 'A') + 27 where

compartmentPriority :: String -> Int
compartmentPriority l = res where
  n = length l
  n2 = n `div` 2
  c1 = take n2 l
  c2 = drop n2 l
  common = intersect c1 c2

  res = case common of
    first:rest -> charPriority first
    _ -> 0
  

part1 :: [String] -> Int
part1 ls = sum where
  priorities = map compartmentPriority ls
  sum = foldl (+) 0 priorities


part2 :: [String] -> Int
part2 ls = sum where
  toPriorities :: String -> String -> String -> Int
  toPriorities x y z = p where
    common = x `intersect` y `intersect` z
    p = charPriority (head common)

  helper (x:y:z:rest) = (toPriorities x y z):(helper rest)
  helper _ = []
  sum = foldl (+) 0 (helper ls)

main = do
  let fileName = "input.dat"
  input <- readFile fileName
  print (part1 (lines input))
  print (part2 (lines input))
