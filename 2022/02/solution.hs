import Data.List.Split (splitOn)

data Options = ROCK | PAPER | SCISSOR deriving Eq
optionScore ROCK = 1
optionScore PAPER = 2
optionScore SCISSOR = 3

data Results = WIN | DRAW | LOSS deriving Eq
resultsScore WIN = 6
resultsScore DRAW = 3
resultsScore LOSS = 0

gameResultToScore :: (Options, Results) -> Int
gameResultToScore (o, r) = optionScore o + resultsScore r

gameRules :: Options -> Options -> Results
gameRules ROCK PAPER = WIN
gameRules PAPER SCISSOR = WIN
gameRules SCISSOR ROCK = WIN
gameRules ROCK ROCK = DRAW
gameRules PAPER PAPER = DRAW
gameRules SCISSOR SCISSOR = DRAW
gameRules _ _ = LOSS

-- If the opponent plays o, what should we play to get the desired result
gameResultToPlay :: Options -> Results -> Options
gameResultToPlay o r 
  | gameRules o ROCK  == r  = ROCK
  | gameRules o PAPER == r  = PAPER
  | otherwise               = SCISSOR

stringToGame :: String -> (Options, Options) 
stringToGame s = res where
  splitted = splitOn " " s
  (p1:p2:rest) = case splitted of
    (t1:t2:rest) -> splitted
    _ -> error "Malformatted line"

  converter :: [Char] -> Options
  converter "A" = ROCK
  converter "X" = ROCK
  converter "B" = PAPER
  converter "Y" = PAPER
  converter _ = SCISSOR

  res = (converter p1, converter p2)


part1 :: [String] -> Int
part1 input_lines = sum where
  gs = map stringToGame input_lines
  games_res = map (\(o1, o2) -> (o2, gameRules o1 o2)) gs
  games_score = map gameResultToScore games_res
  sum = foldl (+) 0 games_score

stringToGameResult :: String -> (Options, Results) 

stringToGameResult s = (p2_play, result) where
  splitted = splitOn " " s
  (p1:p2:rest) = case splitted of
    (t1:t2:rest) -> splitted
    _ -> error "Malformatted line"

  p1_converter :: [Char] -> Options
  p1_converter "A" = ROCK
  p1_converter "B" = PAPER
  p1_converter _ = SCISSOR

  r_converter "X" = LOSS
  r_converter "Y" = DRAW
  r_converter _ = WIN

  p1_play = p1_converter p1
  result = r_converter p2
  p2_play = gameResultToPlay p1_play result

part2 :: [String] -> Int
part2 input_lines = sum where
  games_res = map stringToGameResult input_lines
  games_score = map gameResultToScore games_res
  sum = foldl (+) 0 games_score

main = do
  let fileName = "input.dat"
  input <- readFile fileName
  let input_lines = lines input
  print (part1 input_lines)
  print (part2 input_lines)
