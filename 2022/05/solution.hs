import Data.List.Split (splitOn)
import Data.List

data Move = M Int Int Int deriving Show

parseInput :: String -> ([String],[Move])
parseInput s = (tstack, instructions) where
  (before:_) = splitOn "\n 1" s
  before_splitted = splitOn "\n" before
  every4th (x:y:z:t:rest) = x:(every4th rest)
  every4th (x:y:rest) = x:(every4th rest)
  every4th _ = []
  stack = map (every4th . tail) before_splitted
  tstack = map (\e -> filter (\t -> not(t == ' ')) e) (transpose stack)
  _:after:_ = splitOn "\n\n" s

  move_strings = map (splitOn " ") (lines after)
  instructions = map (\e -> (M (read (e !! 1) :: Int) ((read (e !! 3) :: Int)-1) ((read (e !! 5))-1))) move_strings

part12 :: [String] -> [Move] -> Bool -> String
part12 stack moves is_problem1 = res where
  helper [] s = s
  helper ((M num source dest):ms) s = helper ms after_move_stack where
    toMove = case is_problem1 of
      True -> reverse (take num (s !! source))
      otherwise -> take num (s !! source)

    transformer i (r:rs)
      | i == source        = (drop num r) : (transformer (i+1) rs)
      | i == dest          = (toMove ++ r) : (transformer (i+1) rs)
      | otherwise          = r : (transformer (i+1) rs)
    transformer _ [] = []
    after_move_stack = transformer 0 s
  res = map head (helper moves stack)


main = do
  let fileName = "input.dat"
  input <- readFile fileName
  let (stack, instructions) = parseInput input

  print (part12 stack instructions True)
  print (part12 stack instructions False)
