import Data.List

data INSTRUCTION = ADDX Int | NOOP deriving (Show)

parse :: String -> [INSTRUCTION]
parse s = res where
  helper (i:v:[]) = ADDX (read v)
  helper (i:[]) = NOOP
  helper _ = error "Unexpected string to parse"
  res = map helper . map words . lines $ s

everyNth :: Int -> [Int] -> [Int]
everyNth _ [] = []
everyNth n (x:xs) = x:(everyNth n (drop (n-1) xs))


execute :: [INSTRUCTION] -> [Int]
execute instructions = values where 
  valuesToAdd = concatMap (\i -> case i of
                     ADDX v -> [0, v]
                     NOOP -> [0]) instructions
  values = scanl (+) 1 $ valuesToAdd

part1 :: [INSTRUCTION] -> Int
part1 instructions = res where
  values = execute instructions
  res = sum $ everyNth 40 $ drop 19 $ map (\(x,y) -> x*y) (zip values [1..])

part2 :: [INSTRUCTION] -> String
part2 instructions = res where
  screenWidth = 40
  screenHeight = 6
  values = execute instructions
  render = map (\(i,v) -> if abs ((mod i screenWidth) - v) <= 1 then '#' else '.') (zip [0 ..] values)
  helper "" = ""
  helper s = take screenWidth s ++ "\n" ++ helper (drop screenWidth s)

  res = helper $ take (screenWidth*screenHeight) render

main = do
  input <- readFile "input.dat"
  let instructions = parse(input)
  print (part1 instructions)
  putStr (part2 instructions)
