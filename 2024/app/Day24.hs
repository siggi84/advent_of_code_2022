import Data.Graph
import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)

data Gate
  = AND
  | OR
  | XOR
  deriving (Show, Eq, Ord)

show2d :: Int -> String
show2d n
  | length (show n) == 1 = "0" ++ show n
  | otherwise = show n

data GateOperation =
  GateOperation
    { input1 :: String -- Name of the first input wire.
    , input2 :: String -- Name of the second input wire.
    , output :: String -- Name of the output wire.
    , gate :: Gate -- Type of the gate.
    }
  deriving (Show, Eq)

parseInput s = (Map.fromList ws, rs)
  where
    [wireLs, relationsLs] = splitOn "\n\n" s
    ws :: [(String, Bool)]
    ws = map ((\[k, v] -> (k, v == " 1")) . splitOn ":") (lines wireLs)
    other = map words $ lines relationsLs
    toOperation "AND" = AND
    toOperation "OR" = OR
    toOperation "XOR" = XOR
    toOperation _ = error "Should not happen"
    rs =
      map (\[w1, o, w2, _, w3] -> GateOperation w1 w2 w3 (toOperation o)) other

-- Build the dependency graph.
buildDependencyGraph :: [GateOperation] -> Map String Int -> Graph
buildDependencyGraph gates wireIndex =
  let edges =
        [(wireIndex Map.! input1 g, wireIndex Map.! output g) | g <- gates] ++
        [(wireIndex Map.! input2 g, wireIndex Map.! output g) | g <- gates]
      bounds = (0, Map.size wireIndex - 1)
   in buildG bounds edges

binToDec :: [Bool] -> Int
binToDec =
  foldl
    (\acc x ->
       2 * acc +
       (if x
          then 1
          else 0))
    0

--Decimal to binary
intToBinaryBoolArray :: Int -> [Bool]
intToBinaryBoolArray 0 = [False]
intToBinaryBoolArray n = reverse (toBinary n)
  where
    toBinary 0 = []
    toBinary x = (x `mod` 2 == 1) : toBinary (x `div` 2)

buildGraph (wires, gates) = (g, gateIndex, wireIndexInv, wireIndex)
  where
    allWires :: [String]
    allWires =
      nub $
      Map.keys wires ++ concat [[input1 g, input2 g, output g] | g <- gates]
    wireIndex :: Map.Map String Vertex
    wireIndex = Map.fromList $ zip allWires [0 ..]
    wireIndexInv = Map.fromList $ zip [0 ..] allWires
    gateIndex = Map.fromList [(output g, g) | g <- gates]
    g = buildDependencyGraph gates wireIndex

calculateMap (wires, gates) = res
  where
    (g, gateIndex, wireIndexInv, wireIndex) = buildGraph (wires, gates)
    evalOrder = mapMaybe (`Map.lookup` wireIndexInv) $ topSort g
    f ws vertex =
      case Map.lookup vertex gateIndex of
        Just (GateOperation i1 i2 _ g) ->
          let i1x = Map.findWithDefault False i1 ws
              i2x = Map.findWithDefault False i2 ws
              res =
                case g of
                  AND -> i1x && i2x
                  OR -> i1x || i2x
                  XOR -> i1x /= i2x
           in Map.insert vertex res ws
        Nothing -> ws
    res = foldl f wires evalOrder

part1 (wires, gates) = res'
  where
    res = calculateMap (wires, gates)
    res' =
      binToDec $
      reverse $
      map snd $ sort $ filter (\(k, v) -> head k == 'z') $ Map.toList res

part2 (wires, gates) = resultString
  where
    (g, gateIndex, wireIndexInv, wireIndex) = buildGraph (wires, gates)
    keys = Map.keys wires
    numBits = length keys `div` 2
    zmaxString = 'z' : show2d numBits
    isInvalidOutputGate =
      [ g
      | g <- gates
      , let op = output g
      , head op == 'z'
      , op /= zmaxString
      , gate g /= XOR
      ]
    isInvalidXorGate =
      [ g
      | g <- gates
      , gate g == XOR
      , not (isBit $ output g)
      , not (isBit $ input1 g)
      , not (isBit $ input2 g)
      ]

    isInvalidAndGate =
      [ g
      | g <- gates
      , gate g == AND
      , not (isInputOf "x00" g)
      , sg <- subgates g
      , gate sg /= OR
      ]
    isInvalidXorGate2 =
      [g | g <- gates, gate g == XOR, sg <- subgates g, gate sg == OR]
    allInvalidGates = nub $ isInvalidAndGate ++ isInvalidOutputGate ++ isInvalidXorGate ++ isInvalidXorGate2
    resultString = intercalate "," $ sort $ map output allInvalidGates

    isBit n = head n == 'x' || head n == 'y' || head n == 'z'
    isInputOf n go = (input1 go == n) || (input2 go == n)
    subgates go = filter (isInputOf (output go)) gates

main :: IO ()
main = do
  testInput <- readFile "data/day24_test.txt"
  finalInput <- readFile "data/day24.txt"
  print $ part1 $ parseInput testInput
  print $ part1 $ parseInput finalInput
  print $ part2 $ parseInput finalInput
