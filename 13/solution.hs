import Data.Char
import Data.List
import Data.Maybe

data Packet
  = Number Int
  | List [Packet]
  deriving (Show, Read, Eq)

parseLine :: String -> Packet
parseLine s = read (rewriteLine s)
  where
    rewriteLine :: String -> String
    rewriteLine ('[':cs) = "List [" ++ rewriteLine cs
    rewriteLine (']':cs) = "]" ++ rewriteLine cs
    rewriteLine (',':cs) = "," ++ rewriteLine cs
    rewriteLine [] = ""
    rewriteLine cs = "Number " ++ takeWhile isDigit cs ++ rewriteLine (dropWhile isDigit cs)

parseInput :: String -> [Packet]
parseInput s = map parseLine $ filter (not . null) (lines s)

pcompare :: Packet -> Packet -> Ordering
pcompare (Number p1) (Number p2) = compare p1 p2
pcompare (List []) (List []) = EQ
pcompare (List []) (List cs) = LT
pcompare (List cs) (List []) = GT
pcompare (List cs) (Number n) = pcompare (List cs) (List [Number n])
pcompare (Number n) (List cs) = pcompare (List [Number n]) (List cs)
pcompare (List (h1:r1)) (List (h2:r2)) =
  case pcompare h1 h2 of
    EQ -> pcompare (List r1) (List r2)
    _ -> pcompare h1 h2

part1 :: [Packet] -> Int
part1 packets = sum $ map fst $ filter (\(idx, p) -> p == LT) (zip [1 ..] (go packets))
  where
    go (p1:p2:ps) = pcompare p1 p2 : go ps
    go [] = []
    go _ = error "Odd number of packets"

part2 :: [Packet] -> Int
part2 packets = res
  where
    div1 = List [List [Number 2]]
    div2 = List [List [Number 6]]
    augmented = packets ++ [div1, div2]
    sortedPackets = sortBy pcompare augmented
    div1loc = fromJust (elemIndex div1 sortedPackets)
    div2loc = fromJust (elemIndex div2 sortedPackets)
    res = (div1loc + 1) * (div2loc + 1)

main = do
  input <- readFile "input.dat"
  let parsedInput = parseInput input
  print (part1 parsedInput)
  print (part2 parsedInput)
