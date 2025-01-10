module Main where

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

leaveOneOut :: [a] -> [[a]]
leaveOneOut xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

isSafe :: [Int] -> Bool
isSafe report = allIncreasing || allDecreasing
  where
    deltas = zipWith (-) report (tail report)
    isValidDelta d = abs d >= 1 && abs d <= 3
    allIncreasing = all (\x -> x > 0 && isValidDelta x) deltas
    allDecreasing = all (\x -> x < 0 && isValidDelta x) deltas

numSafeReports :: [[Int]] -> Int
numSafeReports = countIf isSafe

numSafeReportsDampened :: [[Int]] -> Int
numSafeReportsDampened = countIf (\r -> isSafe r || any isSafe (leaveOneOut r))

main :: IO ()
main = do
  input <- readFile "data/day2.txt"
  let reports = parseInput input
  print (numSafeReports reports)
  print (numSafeReportsDampened reports)
