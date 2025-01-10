import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Map (Map)
import Text.Parsec
import Text.Parsec.String (Parser)

number :: Parser Int
number = read <$> do many1 digit

parseRule :: Parser (Int, Int)
parseRule = do
  x <- number
  char '|'
  y <- number
  return (x, y)

parseList :: Parser [Int]
parseList = do
  x <- number
  xs <- many (char ',' >> number)
  return (x : xs)

inputParser :: Parser ([(Int, Int)], [[Int]])
inputParser = do
  rules <- many (parseRule <* char '\n')
  char '\n'
  orderings <- many (parseList <* char '\n')
  return (rules, orderings)

orderingIsValid :: [(Int, Int)] -> [Int] -> Bool
orderingIsValid rules ordering = not $ or rulesViolated
  where
    n = length ordering
    rulesViolated =
      [ (ordering !! j, ordering !! i) `elem` rules
      | i <- [0 .. n - 1]
      , j <- [i + 1 .. n - 1]
      ]

centralValue :: [Int] -> Int
centralValue ordering = ordering !! (length ordering `div` 2)

topologicalSort :: [(Int, Int)] -> [Int]
topologicalSort rs =
  let nodes = nub (concatMap (\(a, b) -> [a, b]) rs)
      edges = M.fromListWith (++) [(a, [b]) | (a, b) <- rs]
      indeg = M.fromListWith (+) [(b, 1) | (_, b) <- rs]
      zeroIndegree = filter (\n -> M.findWithDefault 0 n indeg == 0) nodes
      go [] _ _ = []
      go (n:ns) es ds =
        let successors = M.findWithDefault [] n es
            (es', ds', zero') = foldl reduce (es, ds, []) successors
         in n : go (ns ++ zero') es' ds'
        where
          reduce (eMap, dMap, zs) succN =
            let d' = M.findWithDefault 0 succN dMap - 1
                dMap' =
                  if d' == 0
                    then M.delete succN dMap
                    else M.insert succN d' dMap
                zs' =
                  if d' == 0
                    then succN : zs
                    else zs
             in (eMap, dMap', zs')
      sorted = go zeroIndegree edges indeg
   in if length sorted == length nodes
        then sorted
        else error "Cycle detected: Topological sort not possible."

sortOrdering :: [(Int, Int)] -> [Int] -> [Int]
sortOrdering rules ordering = topOrdering
  where
    relevantRules =
      filter (\(a, b) -> a `elem` ordering && b `elem` ordering) rules
    topOrdering = topologicalSort relevantRules

part1 rules orderings =
  sum $ map centralValue $ filter (orderingIsValid rules) orderings

part2 rules orderings =
  sum $
  [ centralValue (sortOrdering rules o)
  | o <- orderings
  , not (orderingIsValid rules o)
  ]

main :: IO ()
main = do
  input <- readFile "data/day5.txt"
  case parse inputParser "" input of
    Left err -> print err
    Right (rules, orderings) ->
      print $ (part1 rules orderings, part2 rules orderings)
