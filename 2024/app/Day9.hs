import Data.Char
import Data.List (find, findIndex, foldl', sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)

data Block
  = Empty
  | File Int
  deriving (Eq, Show)

parseInput :: String -> [Block]
parseInput s = helper (head $ words s) 0
  where
    helper :: String -> Int -> [Block]
    helper (f:s:rest) i =
      replicate (digitToInt f) (File i) ++
      replicate (digitToInt s) Empty ++ helper rest (i + 1)
    helper [f] i = replicate (digitToInt f) (File i)

part1 blocks = sum $ map (\(i, b) -> i * extract b) $ zip [0 ..] (go blocks)
  where
    extract b =
      case b of
        File n -> n
        Empty -> 0
    go [x] =
      case x of
        File n -> [x]
        Empty -> []
    go (x:xs) =
      case x of
        File n -> x : go xs
        Empty ->
          case lastBlock of
            File n -> lastBlock : go (init xs)
            Empty -> go (x : init xs)
      where
        lastBlock = last (x : xs)

data Segment
  = Segment
      { size :: Int
      , index :: Int
      , position :: Int
      }
  | NoData
      { size :: Int
      , position :: Int
      }
  deriving (Show, Eq)

parseSegments :: String -> [Segment]
parseSegments s = helper (head $ words s) 0 0
  where
    helper :: String -> Int -> Int -> [Segment]
    helper (f:s:rest) i c =
      [Segment fd i c] ++
      [NoData sd (c + fd)] ++ helper rest (i + 1) (c + fd + sd)
      where
        fd = digitToInt f
        sd = digitToInt s
    helper [f] i c = [Segment (digitToInt f) i c]

getFirst :: Ord a => [a] -> Maybe a
getFirst [] = Nothing
getFirst xs = Just $ foldl' max (head xs) (tail xs)

move :: [Segment] -> Segment -> [Segment]
move ses seg =
  case empty of
    Just empty -> helper empty seg beforef afterf
    _ -> ses
  where
    (Segment s i _) = seg
    [beforef, afterf] = splitOn [seg] ses
    emptyLargeEnough (Segment {}) = False
    emptyLargeEnough (NoData s _) = s >= size seg
    empty = find emptyLargeEnough beforef
    helper :: Segment -> Segment -> [Segment] -> [Segment] -> [Segment]
    helper e seg beforef afterf =
      beforeE ++
      [seg] ++
      tmp ++ mergeAdjacent (afterE ++ NoData (size seg) (position seg) : afterf)
      where
        [beforeE, afterE] = splitOn [e] beforef
        remainingSize = size e - size seg
        tmp = [NoData remainingSize (position e) | remainingSize > 0]
    mergeAdjacent ((NoData s1 p1):(NoData s2 p2):xs) =
      mergeAdjacent $ NoData (s1 + s2) p1 : xs
    mergeAdjacent (x:xs) = x : mergeAdjacent xs
    mergeAdjacent [] = []

part2 :: [Segment] -> Int
part2 segments = score
  where
    isFile :: Segment -> Bool
    isFile (Segment {}) = True
    isFile _ = False
    files = filter isFile segments
    res = foldl' move segments (reverse files)
    res' = concatMap repSig res
    repSig (Segment s i p) = replicate s i
    repSig (NoData s p) = replicate s 0
    score = sum $ zipWith (*) [0 ..] res'

main :: IO ()
main
-- Probably alot of room for speed improvements by using a different data structure.
 = do
  input <- readFile "data/day9.txt"
  print $ part1 $ parseInput input
  print $ part2 $ parseSegments input
