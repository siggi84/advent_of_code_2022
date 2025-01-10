import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Set as S

parseInput :: String -> (S.Set String, [String])
parseInput s = (S.fromList towels, drop 2 ls)
  where
    ls = lines s
    towels = splitOn ", " (head ls)

listSubstrings :: S.Set String -> String -> [(Int, Int)]
listSubstrings towels s =
  let n = length s
      m = maximum $ S.toList $ S.map length towels
   in [ (i, j)
      | j <- [1 .. n]
      , i <- [(j-m) .. j]
      , take (j - i) (drop i s) `S.member` towels
      ]

-- Use the fact that the strings form a DAG to count the number of paths
countPaths :: S.Set String -> String -> Int
countPaths towels s = M.findWithDefault 0 n finalCounts
  where
    n = length s
    pairs = listSubstrings towels s
    initialCounts = M.singleton 0 1
    update counts (start, end) = 
      let waysStart = M.findWithDefault 0 start counts
          waysEnd = M.findWithDefault 0 end counts
        in M.insert end (waysEnd + waysStart) counts
    finalCounts = foldl update initialCounts pairs

part1 :: (S.Set String, [String]) -> Int
part1 (towels, designs) = length . filter (> 0) $ map (countPaths towels) designs

part2 :: (S.Set String, [String]) -> Int
part2 (towels, designs) = sum $ map (countPaths towels) designs

main :: IO ()
main = do
  testInput <- readFile "data/day19_test.txt"
  let parsedTestInput = parseInput testInput
  input <- readFile "data/day19.txt"
  let parsedInput = parseInput input
  print $ part1 parsedTestInput
  print $ part1 parsedInput
  print $ part2 parsedTestInput
  print $ part2 parsedInput
