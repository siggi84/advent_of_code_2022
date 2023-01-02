import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

data Monkey
  = NumberMonkey
      { name :: String
      , value :: Int
      }
  | EquationMonkey
      { name :: String
      , left :: String
      , right :: String
      , op :: Int -> Int -> Int
      , leftInv :: Int -> Int -> Int
      , rightInv :: Int -> Int -> Int
      }

type MonkeyCollection = Map.Map String Monkey

type MonkeyName = String

getMonkey :: MonkeyCollection -> MonkeyName -> Monkey
getMonkey monkeys name = fromJust (Map.lookup name monkeys)

parseInput :: String -> MonkeyCollection
parseInput s = Map.fromList $ map handleLine (lines s)
  where
    handleLine l
      | length wl == 2 = (name, NumberMonkey name (read (wl !! 1)))
      | otherwise = (name, EquationMonkey name (wl !! 1) (wl !! 3) equation leftInverse rightInverse)
      where
        wl = words l
        name = takeWhile (/= ':') (head wl)
        sym = wl !! 2
        (equation, leftInverse, rightInverse)
          | sym == "+" = ((+), (-), (-))
          | sym == "*" = ((*), div, div)
          | sym == "-" = ((-), (+), flip (-))
          | sym == "/" = (div, (*), flip div)
          | otherwise = error "Unknown symbol"

evaluate :: MonkeyCollection -> Monkey -> (Int, Bool)
evaluate monkeys (NumberMonkey name value) = (value, name == "humn")
evaluate monkeys m = (res, isHuman)
  where
    ((_, leftValue, leftIsHuman), (_, rightValue, rightIsHuman)) = evaluateParents monkeys m
    (res, isHuman) = (op m leftValue rightValue, leftIsHuman || rightIsHuman)

evaluateParents :: MonkeyCollection -> Monkey -> ((Monkey, Int, Bool), (Monkey, Int, Bool))
evaluateParents monkeys (NumberMonkey _ _) = error "Should only be used for EquationMonkeys"
evaluateParents monkeys m = ((leftMonkey, leftValue, leftIsHuman), (rightMonkey, rightValue, rightIsHuman))
  where
    leftMonkey = getMonkey monkeys (left m)
    rightMonkey = getMonkey monkeys (right m)
    (leftValue, leftIsHuman) = evaluate monkeys leftMonkey
    (rightValue, rightIsHuman) = evaluate monkeys rightMonkey

part1 :: MonkeyCollection -> Int
part1 monkeys = fst (evaluate monkeys (getMonkey monkeys "root"))

part2 :: MonkeyCollection -> Int
part2 monkeys = backtrack newRoot 1
  where
    root = getMonkey monkeys "root"
    equalityInverse 1 a = a
    equalityInverse _ _ = error "No inverse of false"
    newRoot =
      case root of
        EquationMonkey name l r _ _ _ ->
          EquationMonkey name l r (\a b -> fromEnum (a == b)) equalityInverse equalityInverse
        _ -> error "The root should be an EqualityMonkey!"
    backtrack (NumberMonkey _ _) result = result
    backtrack m result =
      if leftIsHuman
        then leftBacktrack
        else rightBacktrack
      where
        ((leftMonkey, leftValue, leftIsHuman), (rightMonkey, rightValue, rightIsHuman)) = evaluateParents monkeys m
        leftBacktrack = backtrack leftMonkey ((leftInv m) result rightValue)
        rightBacktrack = backtrack rightMonkey ((rightInv m) result leftValue)

main = do
  inputString <- readFile "input.dat"
  let parsedInput = parseInput inputString
  print (part1 parsedInput)
  print (part2 parsedInput)
