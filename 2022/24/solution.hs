{-# LANGUAGE TupleSections #-}
import Data.List
import qualified Data.Set as Set

data Coord =
  C Int Int
  deriving (Eq, Ord, Show)

addCoord (C x1 y1) (C x2 y2) = C (x1 + x2) (y1 + y2)

data Direction
  = U
  | D
  | L
  | R
  deriving (Eq, Ord, Show)

getX (C x _) = x

getY (C _ y) = y

parseInput :: String -> (Int, Int, Set.Set (Coord, Direction))
parseInput s = (n, m, Set.fromList blizzards)
  where
    ls = lines s
    n = length ls - 2
    m = length (head ls) - 2
    charToDirectin '^' = U
    charToDirectin '>' = R
    charToDirectin '<' = L
    charToDirectin 'v' = D
    charToDirection _ = error "Unexpected symbol"
    blizzards = concat $ zipWith handleLine [(-1) ..] ls
    handleLine lIdx l =
      map (\(coord, c) -> (coord, charToDirectin c)) $
      filter (\(_, c) -> c `elem` "><^v") $ zipWith (\c cIdx -> (C lIdx cIdx, c)) l [(-1) ..]

evolve width height = Set.map move
  where
    move (C r c, U) = (C (mod (r - 1) height) c, U)
    move (C r c, D) = (C (mod (r + 1) height) c, D)
    move (C r c, L) = (C r (mod (c - 1) width), L)
    move (C r c, R) = (C r (mod (c + 1) width), R)

precalculateSeq width height blizzards = blizzardsSeq
  where
    steps = width*height `div` gcd width height
    evolveWH = evolve width height
    blizzardsSeq = take steps $ iterate evolveWH blizzards

getBlizzard blizzardsSeq t = blizzardsSeq !! mod t (length blizzardsSeq)

bfs width height start stop q blizzardsSeq explored =
  if current == stop
    then t
    else bestT
  where
    (t, current) = head q
    nextBlizzard = getBlizzard blizzardsSeq (t + 1)
    steps = [C 0 1, C 1 0, C 0 0, C 0 (-1), C (-1) 0]
    noBlizzard coord = not $ any (\d -> Set.member (coord, d) nextBlizzard) [U, D, L, R]
    inGrid coord = (0 <= r && r < height && 0 <= c && c < width) || coord == start || coord == stop
      where
        (C r c) = coord
    nextLocations =
      filter (\e -> not (Set.member e explored)) $ 
      map (t + 1,) $
      filter noBlizzard $
      filter inGrid $
      map (addCoord current) steps
    updatedQ = tail q ++ nextLocations
    updatedExplored = Set.union explored (Set.fromList nextLocations)
    bestT = bfs width height start stop updatedQ blizzardsSeq updatedExplored

part1 (height, width, blizzards) = res
  where
    evolveWH = evolve width height
    blizzardsSeq = precalculateSeq width height blizzards
    start = C (-1) 1
    stop = C height (width - 1)
    res = bfs width height start stop [(0, start)] blizzardsSeq (Set.fromList [(0, start)])

part2 (height, width, blizzards) = t2
  where
    evolveWH = evolve width height
    blizzardsSeq = precalculateSeq width height blizzards
    start = C (-1) 1
    stop = C height (width - 1)
    t0 = bfs width height start stop [(0, start)] blizzardsSeq (Set.fromList [(0, start)])
    t1 = bfs width height stop start [(t0, stop)] blizzardsSeq (Set.fromList [(t0, stop)])
    t2 = bfs width height start stop [(t1, start)] blizzardsSeq (Set.fromList [(t1, start)])

main = do
  inputString <- readFile "input.dat"
  let parsedInput = parseInput inputString
  print (part1 parsedInput)
  print (part2 parsedInput)
