import Data.List.Split (splitOn)
import Data.List

data Monkey = Monkey {  items :: [Int],
                        op :: (Int -> Int),
                        divBy :: Int,
                        monkeyTrue :: Int,
                        monkeyFalse :: Int,
                        inspection :: Int
                     }

parseMonkey :: String -> Monkey
parseMonkey s = monkey where
  ls = map words $ lines s
  items = map read $ drop 2 $ (words) $ filter (\c -> c /= ',') ((lines s) !! 1)
  divValue = read ((ls !! 3) !! 3) :: Int
  monkeyT = read ((ls !! 4) !! 5) :: Int
  monkeyF = read ((ls !! 5) !! 5) :: Int
  op :: Int -> Int -> Int
  op = if ((ls !! 2) !! 4) == "+" then (+) else (*)
  arg1 = (ls !! 2) !! 3
  arg2 = (ls !! 2) !! 5
  f = (\d -> op (if arg1 == "old" then d else (read arg1 :: Int)) (if arg2 == "old" then d else (read arg2 :: Int)))
  monkey = Monkey items f divValue monkeyT monkeyF 0

evolver :: [Monkey] -> Int -> Int -> Int
evolver monkeys num_rounds divider = res where 
  num_monkeys = length monkeys
  modVal = product $ nub $ map divBy monkeys

  helper :: [Monkey] -> Int -> [Monkey]
  helper ms idx = updated_monkey_list where
    idxMod = mod idx num_monkeys
    monkey = ms !! idxMod

    items_updated = map (\v -> mod (op monkey v `div` divider) modVal) $ items monkey

    true_monkey = (ms !! (monkeyTrue monkey))
    true_m = filter (\v -> mod v (divBy monkey) == 0) items_updated

    false_monkey = (ms !! (monkeyFalse monkey))
    false_m = filter (\v -> mod v (divBy monkey) /= 0) items_updated

    updated_monkey = monkey { items = [], inspection = (inspection monkey) + (length items_updated) }
    true_monkey_updated = true_monkey { items = (items true_monkey) ++ true_m}
    false_monkey_updated = false_monkey { items = (items false_monkey) ++ false_m}
    updated_monkey_list = map (\(i,m) -> if (i == idxMod) then updated_monkey
                         else if (i == (monkeyTrue monkey)) then true_monkey_updated
                         else if (i == (monkeyFalse monkey)) then false_monkey_updated
                         else m) (zip [0..] ms)

  
  updated_monkeys = foldl helper monkeys [0 .. (num_monkeys*num_rounds-1)]
  activity = reverse $ sort $ map inspection updated_monkeys
  res = (activity !! 0) * (activity !! 1)

part1 :: [Monkey] -> Int
part1 monkeys = evolver monkeys 20 3

part2 :: [Monkey] -> Int
part2 monkeys = evolver monkeys 10000 1

parseInput :: String -> [Monkey]
parseInput s = map parseMonkey $ splitOn "\n\n" s

main = do
  input <- readFile "input.dat"
  let parsedInput = parseInput input
  

  print (part1 parsedInput)
  print (part2 parsedInput)
