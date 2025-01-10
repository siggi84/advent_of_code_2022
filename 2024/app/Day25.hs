import Data.Function (on)
import Data.List (groupBy, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

data Schematic
  = Lock [Int]
  | Key [Int]
  deriving (Show)

findMinMaxX :: [(Int, Int)] -> [(Int, (Int, Int))]
findMinMaxX tuples =
  let sorted = sortBy (compare `on` snd) tuples
      grouped = groupBy ((==) `on` snd) sorted
   in map processGroup grouped
  where
    processGroup group@((_, y):_) =
      let xs = map fst group
       in (y, (minimum xs, maximum xs))
    processGroup [] = error "Unexpected empty group"

parseInput s = map parseSchematic schematics
  where
    schematics = splitOn "\n\n" s
    parseSchematic schematic
      | isLock = Lock lockPins
      | otherwise = Key keyPins
      where
        ls = lines schematic
        numLines = length ls
        isLock = all (== '#') $ head ls
        ns = findMinMaxX $ locations '#'
        lockPins = map (snd . snd) ns
        keyPins = map ((\c -> numLines - c - 1) . fst . snd) ns
        locations c =
          [ (lineIndex, charIndex)
          | (lineIndex, line) <- zip [0 ..] ls
          , (charIndex, c') <- zip [0 ..] line
          , c' == c
          ]

part1 d = numMatches
  where
    numMatches = sum $ [1 | s1 <- d, s2 <- d, match s1 s2]
    match (Key k) (Lock l) = all (\(k', l') -> k' + l' <= 5) $ zip k l
    match _ _ = False

main :: IO ()
main = do
  testInput <- readFile "data/day25_test.txt"
  realInput <- readFile "data/day25.txt"
  let testData = parseInput testInput
  let realData = parseInput realInput
  print $ part1 testData
  print $ part1 realData
