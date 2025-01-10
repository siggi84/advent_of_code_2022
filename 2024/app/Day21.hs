import Data.Char (isDigit)
import qualified Data.Map.Strict as M

data Direction
  = A
  | U
  | R
  | D
  | L
  deriving (Show, Enum, Eq, Ord)

data Numpad
  = NA
  | N0
  | N1
  | N2
  | N3
  | N4
  | N5
  | N6
  | N7
  | N8
  | N9
  deriving (Show, Enum, Eq, Ord)

type Coord = (Int, Int)

move :: Coord -> Direction -> Coord
move (x, y) U = (x + 1, y)
move (x, y) D = (x - 1, y)
move (x, y) L = (x, y - 1)
move (x, y) R = (x, y + 1)
move (x, y) A = (x, y)

computeMoves :: Coord -> Coord -> [Direction]
computeMoves (x1, y1) (x2, y2) = moves
  where
    dx = x2 - x1
    dxAbs = abs dx
    dy = y2 - y1
    dyAbs = abs dy
    moves =
      (if dy < 0
         then replicate dyAbs L
         else []) ++
      (if dx < 0
         then replicate dxAbs D
         else []) ++
      (if dx > 0
         then replicate dxAbs U
         else []) ++
      (if dy > 0
         then replicate dyAbs R
         else [])

genpad :: (Enum a) => Coord -> [Coord] -> a -> a -> [Direction]
genpad gap cs n1 n2 =
  if legal
    then moves
    else reverse moves
  where
    c1 = cs !! fromEnum n1
    c2 = cs !! fromEnum n2
    legal = gap `notElem` scanl move c1 moves
    moves = computeMoves c1 c2

numpad :: Numpad -> Numpad -> [Direction]
numpad = genpad gap cs
  where
    gap = (0, 0)
    cs =
      [ (0, 2)
      , (0, 1)
      , (1, 0)
      , (1, 1)
      , (1, 2)
      , (2, 0)
      , (2, 1)
      , (2, 2)
      , (3, 0)
      , (3, 1)
      , (3, 2)
      ]

dirpad :: Direction -> Direction -> [Direction]
dirpad = genpad gap cs
  where
    gap = (1, 0)
    cs = [(1, 2), (1, 1), (0, 2), (0, 1), (0, 0)]

numpadGraph n1 n2 = numpad n1 n2 ++ [A]

dirGraph d1 d2 = dirpad d1 d2 ++ [A]

type Memo = M.Map ([Direction], Int) Int

getLength' :: Memo -> [Direction] -> Int -> (Int, Memo)
getLength' memo seq 0 = (length seq, memo)
getLength' memo seq iter =
  case M.lookup (seq, iter) memo of
    Just result -> (result, memo)
    Nothing -> (total, newMemo)
  where
    seq' = A : seq
    (total, updatedMemo) =
      foldl
        (\(acc, m) (n1, n2) ->
           let (len, m') = getLength' m (dirGraph n1 n2) (iter - 1)
            in (acc + len, m'))
        (0, memo)
        (zip seq' (tail seq'))
    newMemo = M.insert (seq, iter) total updatedMemo

getLength :: [Numpad] -> Int -> Int
getLength seq 0 = length seq
getLength seq iter = total
  where
    seq' = NA : seq
    (_, total) =
      foldl
        (\(memo, acc) (n1, n2) ->
           let (len, updatedMemo) =
                 getLength' memo (numpadGraph n1 n2) (iter - 1)
            in (updatedMemo, acc + len))
        (M.empty, 0)
        (zip seq' (tail seq'))

pinProcess :: String -> (Int, [Numpad])
pinProcess pin = (numval, pin')
  where
    numval = read $ filter isDigit pin
    toNumpad 'A' = NA
    toNumpad c = toEnum $ read [c] + 1
    pin' = map toNumpad pin

part1 d = sum $ map (go . pinProcess) d
  where
    go (numval, pin) = getLength pin 3 * numval

part2 d = sum $ map (go . pinProcess) d
  where
    go (numval, pin) = getLength pin 26 * numval

main :: IO ()
main = do
  let testData = ["029A", "980A", "179A", "456A", "379A"]
  let finalData = ["964A", "140A", "413A", "670A", "593A"]
  -- print $ part1 testData
  print $ part1 testData
  print $ part1 finalData
  print $ part2 testData
  print $ part2 finalData
