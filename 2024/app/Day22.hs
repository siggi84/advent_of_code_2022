import Data.Bits (xor)
import Data.List
import qualified Data.Map.Strict as M

parseInput :: String -> [Int]
parseInput s = map read $ lines s

windows4 (x1:x2:x3:x4:rest) = (x1, x2, x3, x4) : windows4 (x2 : x3 : x4 : rest)
windows4 _ = []

step :: Int -> Int
step n = n'''
  where
    mix = xor
    prune n = mod n 16777216
    n' = prune $ mix (n * 64) n
    n'' = prune $ mix (div n' 32) n'
    n''' = prune $ mix (n'' * 2048) n''

secretNumbers = iterate step

part1 ns = sum $ map go ns
  where
    go n = secretNumbers n !! 2000

part2 ns = res
  where
    numPriceChanges = 2000
    prices n = map (`mod` 10) $ secretNumbers n
    priceChanges n =
      let pc = prices n
       in take numPriceChanges $ zipWith (-) (tail pc) pc
    seqToPrice n = zip (windows4 $ priceChanges n) (drop 4 $ prices n)
    seqToBanana n = foldr insertIfAbsent M.empty $ seqToPrice n
      where
        insertIfAbsent (key, value) acc = M.insertWith const key value acc
    res = maximum $ M.elems $ M.unionsWith (+) $ map seqToBanana ns

main :: IO ()
main = do
  testInput <- readFile "data/day22_test.txt"
  finalInput <- readFile "data/day22.txt"
  let parsedTestInput = parseInput testInput
  let parsedFinalInput = parseInput finalInput
  print $ part1 parsedTestInput
  print $ part1 parsedFinalInput
  print $ part2 parsedTestInput
  print $ part2 parsedFinalInput
