import Data.List
import Data.Maybe
import Debug.Trace

parseInput :: String -> [(Int, Int)]
parseInput s = zip [0 ..] $ map read (lines s)

moveEntry :: [(Int, Int)] -> Int -> [(Int, Int)]
moveEntry ns originalIndex
  | vMod == 0 = ns
  | otherwise = newList
  where
    entryIndex = fromJust (findIndex (\(i, j) -> i == originalIndex) ns)
    entry = ns !! entryIndex
    (_, v) = entry
    nsLen = length ns
    vMod = mod v (nsLen - 1)
    listWithoutEntry = take (nsLen - 1) $ drop (entryIndex + 1) (cycle ns)
    newList = take nsLen (entry : drop vMod (cycle listWithoutEntry))

partx key ns reps = sum $ map (\i -> snd (mixedn !! mod (i + zeroIndex) nsLen)) [1000, 2000, 3000]
  where
    nsLen = length ns
    startNs = map (\(i, j) -> (i, j * key)) ns
    repeatMix = foldl moveEntry
    mixedn = foldl repeatMix startNs (replicate reps [0..nsLen-1])
    zeroIndex = fromJust (findIndex (\(_, v) -> v == 0) mixedn)

part1 :: [(Int, Int)] -> Int
part1 ns = partx 1 ns 1

part2 :: [(Int, Int)] -> Int
part2 ns = partx 811589153 ns 10

main = do
  inputString <- readFile "input.dat"
  let parsedInput = parseInput inputString
  print (part1 parsedInput)
  print (part2 parsedInput)
