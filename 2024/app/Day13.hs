import Control.Monad (guard)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ratio (Ratio, (%), denominator, numerator)
import Text.Parsec
import Text.Parsec.String (Parser)

number :: Parser Integer
number = read <$> do many1 digit

data Button =
  Button Integer Integer
  deriving (Show)

data Prize =
  Prize Integer Integer
  deriving (Show)

data Machine =
  Machine Button Button Prize
  deriving (Show)

parseButton :: Parser Button
parseButton = do
  string "Button "
  oneOf "AB"
  string ": X+"
  x <- number
  string ", Y+"
  y <- number
  return $ Button x y

parsePrize :: Parser Prize
parsePrize = do
  string "Prize: X="
  x <- number
  string ", Y="
  y <- number
  return $ Prize x y

parseMachine :: Parser Machine
parseMachine = do
  buttonA <- parseButton
  char '\n'
  buttonB <- parseButton
  char '\n'
  prize <- parsePrize
  char '\n'
  char '\n'
  return $ Machine buttonA buttonB prize

parseMachines :: Parser [Machine]
parseMachines = do
  many parseMachine <* eof

winPrize (Machine (Button xa ya) (Button xb yb) (Prize xp yp)) = do
  guard (det /= 0)
  let xs = (yb * xp - xb * yp) % det
      ys = (-ya * xp + xa * yp) % det
  guard (denominator xs == 1 && denominator ys == 1)
  guard (numerator xs >= 0 && numerator ys >= 0)
  pure (numerator xs * 3 + numerator ys)
  where
    det = xa * yb - xb * ya

part1 :: [Machine] -> Integer
part1 machines = sum $ mapMaybe winPrize machines

part2 :: [Machine] -> Integer
part2 machines = sum $ mapMaybe winPrize machines'
  where
    delta = 10000000000000
    translateMachine (Machine ba bb (Prize xp yp)) =
      Machine ba bb (Prize (xp + delta) (yp + delta))
    machines' = map translateMachine machines

main :: IO ()
main = do
  input <- readFile "data/day13.txt"
  case parse parseMachines "" input of
    Left err -> print err
    Right machines -> print (part1 machines, part2 machines)
