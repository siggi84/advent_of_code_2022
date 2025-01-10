import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Text.Parsec
import Text.Parsec.String (Parser)

-- Function to parse the input string
parseInput :: String -> [(Integer, [Integer])]
parseInput input = mapMaybe parseLine (lines input)
    -- Parse a single line
  where
    parseLine :: String -> Maybe (Integer, [Integer])
    parseLine line
        -- Split the line into key and values
     = do
      let (keyPart, rest) = break (== ':') line
      rest' <- stripPrefix ": " rest
      key <- readMaybe keyPart
      values <- mapM readMaybe (words rest')
      return (key, values)

-- Helper to safely read integers
readMaybe :: Read a => String -> Maybe a
readMaybe s =
  case reads s of
    [(val, "")] -> Just val
    _ -> Nothing

helper relations operators =
  sum $ map (fst . snd) $ filter fst $ zip hasSol relations
  where
    go acc res [] _ = acc == res
    go acc res (x:xs) ops = or [go (op acc x) res xs ops | op <- ops]
    hasSol = map (\(r, ns) -> go (head ns) r (tail ns) operators) relations

combine :: Integer -> Integer -> Integer
combine n1 n2 = read (show n1 ++ show n2)

part1 relations = helper relations [(+), (*)]

part2 relations = helper relations [(+), (*), combine]

main :: IO ()
main = do
  input <- readFile "data/day7.txt"
  print $ part1 $ parseInput input
  print $ part2 $ parseInput input
