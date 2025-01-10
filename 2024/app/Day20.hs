import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Set as S

-- | Perform a BFS from 'start' to 'goal', using the 'neighbors' function
--   to expand nodes. Returns 'Just path' if a path exists, or 'Nothing'
--   otherwise.
bfs ::
     (Ord a)
  => (a -> [a]) -- ^ neighbors function
  -> a -- ^ start node
  -> a -- ^ goal node
  -> Maybe [a]
bfs neighbors start goal = search [[start]] (Set.singleton start)
  where
    search [] _ = Nothing
    search (path:paths) visited =
      let current = last path
       in if current == goal
            then Just path
            else let nxt = filter (`Set.notMember` visited) (neighbors current)
                     visited' = foldr Set.insert visited nxt
                     newPaths = [path ++ [n] | n <- nxt]
                  in search (paths ++ newPaths) visited'

parseInput s = (start, end, trackNodes, wallNodes)
  where
    locations c =
      [ (lineIndex, charIndex)
      | (lineIndex, line) <- zip [0 ..] (lines s)
      , (charIndex, c') <- zip [0 ..] line
      , c' == c
      ]
    trackNodes = locations '.'
    wallNodes = locations '#'
    [start] = locations 'S'
    [end] = locations 'E'

countCheats (start, end, trackNodes, wallNodes) m lim = cheats
  where
    allNodes = start : end : trackNodes
    trackNodesSet = S.fromList allNodes
    wallNodesSet = S.fromList wallNodes
    nbs (x, y) =
      filter
        (`S.member` trackNodesSet)
        [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    path = Data.Maybe.fromMaybe [] $ bfs nbs start end
    mhdist (x, y) (x', y') = abs (x - x') + abs (y - y')
    n = length path
    cheats =
      length
        [ d
        | (i, n1) <- zip [0 ..] path
        , (j, n2) <- zip [0 ..] path
        , j - i >= lim + 1
        , let d = mhdist n1 n2
        , d <= m
        , (j - i) - d >= lim -- ps saved
        ]

main :: IO ()
main = do
  testInput <- readFile "data/day20_test.txt"
  let parsedTestInput = parseInput testInput
  input <- readFile "data/day20.txt"
  let parsedInput = parseInput input
  print (countCheats parsedInput 2 100, countCheats parsedInput 20 100)
