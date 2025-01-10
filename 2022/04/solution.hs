import Data.List.Split (splitOn)

data Interval = I Int Int deriving Show

parseInterval :: String -> Interval
parseInterval s = (I low high) where
  splitted = splitOn "-" s
  (low,high) = case splitted of
    (s1:s2:[]) -> ((read s1 :: Int), (read s2 :: Int))
    _ -> error "Malformatted interval"

parseLine :: String -> (Interval, Interval)
parseLine s = (i1, i2) where
  splitted = splitOn "," s
  (i1,i2) = case splitted of
    (s1:s2:[]) -> (parseInterval s1, parseInterval s2)
    _ -> error "Malformatted line"

intervalContains :: Interval -> Interval -> Bool
intervalContains (I l1 h1) (I l2 h2) = l1 <= l2 && h2 <= h1

intervalsOverlap :: Interval -> Interval -> Bool
intervalsOverlap (I l1 h1) (I l2 h2) = not (h2 < l1 || l2 > h1)

part :: (Interval -> Interval -> Bool) -> [(Interval, Interval)] -> Int
part fn ss = sum where
  helper (i1, i2) = fromEnum (fn i1 i2)
  tmp = map helper ss
  sum = foldl (+) 0 tmp

part1 :: [(Interval, Interval)] -> Int
part1 ss = part (\i1 i2 -> intervalContains i1 i2 || intervalContains i2 i1) ss

part2 :: [(Interval, Interval)] -> Int
part2 ss = part intervalsOverlap ss

main = do
  let fileName = "input.dat"
  input <- readFile fileName
  let input_lines = lines input
  let intervals = map parseLine input_lines
  print (part1 intervals)
  print (part2 intervals)
