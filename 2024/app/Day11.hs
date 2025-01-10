import Data.Array
import Data.Char
import Data.List
import qualified Data.Map.Strict as M

type Memo = M.Map (Int, Int) Int

intNumDigits :: Int -> Int
intNumDigits 0 = 1
intNumDigits i = go i 0
  where
    go 0 acc = acc
    go n acc = go (n `div` 10) (acc + 1)

splitEven :: Int -> (Int, Int)
splitEven x =
  let d = intNumDigits x
      half = d `div` 2
      pow = 10 ^ half
      left = x `div` pow
      right = x `mod` pow
   in (left, right)

numStones :: Int -> Memo -> Int -> (Int, Memo)
numStones 0 memo x = (1, memo)
numStones n memo 0 = numStones (n - 1) memo 1
numStones n memo x =
  case M.lookup (n, x) memo of
    Just v -> (v, memo)
    Nothing -> (r, M.insert (n, x) r memor)
  where
    (x1, x2) = splitEven x
    (g1, memo1) = numStones (n - 1) memo x1
    (g2, memo2) = numStones (n - 1) memo1 x2
    g3 = numStones (n - 1) memo (x * 2024)
    (r, memor) =
      if even (intNumDigits x)
        then (g1 + g2, memo2)
        else g3

main :: IO ()
main = do
  input <- readFile "data/day11.txt"
  let stones = map read $ words input
  print $ sum $ map (fst . numStones 25 M.empty) stones
  print $ sum $ map (fst . numStones 75 M.empty) stones
