import Data.List.Split (splitOn)
import Data.List (sort)

main :: IO()

convert_to_ints :: [String] -> [Int]
convert_to_ints ls = map read ls

main = do
  input <- readFile "input.dat"
  let input_lines = lines input

  let intChunks = map convert_to_ints (splitOn [""] input_lines)
  let elfCalories = reverse $ sort $ map sum intChunks
  print $ head elfCalories
  print $ sum $ take 3 $ elfCalories
