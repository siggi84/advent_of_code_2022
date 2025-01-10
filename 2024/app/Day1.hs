module Main where

import Data.List (group, isPrefixOf, sort)
import Data.Map (elems, fromListWith, intersectionWithKey, mapWithKey)

parseInput :: String -> ([Int], [Int])
parseInput text = (firstList, secondList)
  where
    ls = map words $ lines text
    firstList = map (read . head) ls
    secondList = map (read . last) ls

calculateScore :: [Int] -> [Int] -> Int
calculateScore firstList secondList =
  sum $ zipWith (\v1 v2 -> abs (v1 - v2)) (sort firstList) (sort secondList)

calculateScore2 :: [Int] -> [Int] -> Int
calculateScore2 firstList secondList =
  sum $ elems $ intersectionWithKey (\k v1 v2 -> k * v1 * v2) countsSL countsFL
  where
    genMap = fromListWith (+) . map (\v -> (v, 1))
    countsFL = genMap firstList
    countsSL = genMap secondList

main :: IO ()
main = do
  input <- readFile "data/day1.txt"
  let (firstList, secondList) = parseInput input
  print $ calculateScore firstList secondList
  print $ calculateScore2 firstList secondList
