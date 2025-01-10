import Data.Graph (SCC(..), stronglyConnComp)
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Tree (flatten)

type Node = String

type Graph = M.Map Node [Node]

parseInput :: String -> [(String, String)]
parseInput s = map ((\[x1, x2] -> (x1, x2)) . splitOn "-") $ lines s

createGraph :: [(Node, Node)] -> Graph
createGraph = foldl' go M.empty
  where
    go g (c1, c2) = M.insertWith (++) c2 [c1] $ M.insertWith (++) c1 [c2] g

bronKerbosch ::
     Int -> [String] -> [String] -> [String] -> Graph -> (Int, [String])
bronKerbosch largest r p x g
  | null p && null x = (max largest (length r), r)
  | otherwise = foldl go (largest, []) (p \\ neighbors pivot)
  where
    pivot = head (p ++ x)
    neighbors node = M.findWithDefault [] node g
    go (currentMax, maxClique) v =
      let (newMax, newClique) =
            bronKerbosch
              currentMax
              (v : r)
              (p `intersect` neighbors v)
              (x `intersect` neighbors v)
              g
       in if newMax > currentMax
            then (newMax, newClique)
            else (currentMax, maxClique)

findLargestClique :: Graph -> [Node]
findLargestClique g = snd $ bronKerbosch 0 [] (M.keys g) [] g

part1 :: [(Node, Node)] -> Int
part1 d = length res
  where
    g = createGraph d
    startsWithT (x:xs) = x == 't'
    startsWithT _ = False
    tNodes = filter startsWithT (M.keys g)
    triNodes n1 =
      nub $
      [ sort [n1, n2, n3]
      | n2 <- M.findWithDefault [] n1 g
      , n1 /= n2
      , n3 <- M.findWithDefault [] n1 g
      , n2 /= n3
      , n3 /= n1
      , n2 `elem` M.findWithDefault [] n3 g
      ]
    res = nub $ concat $ nub $ map triNodes tNodes

part2 :: [(Node, Node)] -> String
part2 d = res
  where
    g = createGraph d
    res = intercalate "," $ sort $ findLargestClique g

main :: IO ()
main = do
  testInput <- readFile "data/day23_test.txt"
  finalInput <- readFile "data/day23.txt"
  let parsedTestInput = parseInput testInput
  let parsedFinalInput = parseInput finalInput
  print $ part1 parsedTestInput
  print $ part1 parsedFinalInput
  print $ part2 parsedTestInput
  print $ part2 parsedFinalInput
