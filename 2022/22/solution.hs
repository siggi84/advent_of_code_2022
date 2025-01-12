import Data.Char
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe

data Instruction
  = L
  | R
  | M Int
  deriving (Show)

data Tile
  = GROUND
  | STONE
  | EMPTY
  deriving (Show, Eq)

data Coord =
  C Int Int
  deriving (Eq, Ord, Show)

directions = [C 0 1, C 1 0, C 0 (-1), C (-1) 0]
numDirections = 4

getX (C x _) = x

getY (C _ y) = y

addCoord (C x1 y1) (C x2 y2) = C (x1 + x2) (y1 + y2)

data CubeFace =
  CubeFace
    { value :: Int
    , rotation :: Int
    }
  deriving (Eq, Ord, Show)

rotateCubeFace (CubeFace v r) rot = CubeFace v (mod (r + rot) numDirections)

data GridMap =
  GridMap
    { grid :: [[Tile]]
    , numRows :: Int
    , numCols :: Int
    , blockSize :: Int
    , instructions :: [Instruction]
    , initCoord :: Coord
    }

parse :: String -> GridMap
parse s = gridMap
  where
    sl = splitOn "\n\n" s
    numCols = maximum $ map length unpaddedGrid
    numRows = length unpaddedGrid
    numNoneGround = length $ filter (/= EMPTY) $ concat grid
    blockSize = (floor . sqrt . fromIntegral) (numNoneGround `div` 6)

    gridMap = GridMap grid numRows numCols blockSize instructions initCoord
    initCoord = C 0 (fromJust (elemIndex GROUND (head grid)))
    grid = map (\r -> take numCols (r ++ repeat EMPTY)) unpaddedGrid

    unpaddedGrid = map (map gridHelper) $ lines $ head sl
    gridHelper ' ' = EMPTY
    gridHelper '.' = GROUND
    gridHelper '#' = STONE

    instructions = helper (sl !! 1)
    helper (c:cs)
      | c == 'L' = L : helper cs
      | c == 'R' = R : helper cs
      | otherwise = M (read (takeWhile isDigit (c : cs))) : helper (dropWhile isDigit cs)
    helper [] = []

evaluate findNext gridMap = (finalX + 1) * 1000 + numDirections * (finalY + 1) + finalD
  where
    (GridMap grid numRows numCols _ instructions initCoord) = gridMap
    (C finalX finalY, finalD) = foldl move (initCoord, 0) instructions
    move (initCoord, dindex) L = (initCoord, mod (dindex - 1) numDirections)
    move (initCoord, dindex) R = (initCoord, mod (dindex + 1) numDirections)
    move (initCoord, dindex) (M n) = nextGridPoint
      where
        (nextCoord, nextDindex) = findNext (initCoord, dindex)
        gridPoint = (grid !! getX nextCoord) !! getY nextCoord

        nextGridPoint
          | n == 0 = (initCoord, dindex)
          | gridPoint == STONE = (initCoord, dindex)
          | gridPoint == GROUND = move (nextCoord, nextDindex) (M (n - 1))
          | gridPoint == EMPTY = error "Cannot travel to an empty square."

part1 :: GridMap -> Int
part1 gridMap = evaluate findNext gridMap
  where
    (GridMap grid numRows numCols _ instructions initCoord) = gridMap
    findNext (initCoord, dindex) = (nextCoord, nextDindex)
      where
        direction = directions !! dindex
        nextCoord =
          head $
          dropWhile (\(C r c) -> ((grid !! r) !! c) == EMPTY) $
          tail $ 
          iterate (\(C r c) -> C (mod (r + getX direction) numRows) (mod (c + getY direction) numCols)) initCoord
        nextDindex = dindex

cubeEdgesHalf =
  [ (CubeFace 1 0, CubeFace 2 3)
  , (CubeFace 1 1, CubeFace 4 2)
  , (CubeFace 1 2, CubeFace 5 3)
  , (CubeFace 2 0, CubeFace 3 3)
  , (CubeFace 2 1, CubeFace 6 2)
  , (CubeFace 3 0, CubeFace 1 3)
  , (CubeFace 3 1, CubeFace 5 2)
  , (CubeFace 3 2, CubeFace 6 3)
  , (CubeFace 4 0, CubeFace 6 1)
  , (CubeFace 4 3, CubeFace 2 2)
  , (CubeFace 5 0, CubeFace 4 1)
  , (CubeFace 5 1, CubeFace 6 0)
  ]

cubeEdges = Map.fromList $ cubeEdgesHalf ++ map (\v -> (snd v, fst v)) cubeEdgesHalf

foldCube (GridMap grid numRows numCols blockSize _ _) = assignFaces blocksMapInit [firstBlock]
  where
    numRowBlocks = div numRows blockSize
    numColBlocks = div numCols blockSize
    blocks =
      [ C (div r blockSize) (div c blockSize)
      | r <- [0,blockSize .. numRows - 1]
      , c <- [0,blockSize .. numCols - 1]
      , (grid !! r) !! c /= EMPTY
      ]
    firstBlock = head blocks
    blocksMapInit = Map.fromList [(firstBlock, CubeFace 1 0)]
    assignFaces blocksMap nextSeq
      | length blocksMap == length blocks = blocksMap
      | otherwise                         = assignFaces updatedBlocksMap updatedNextSeq
      where
        currentBlock = head nextSeq
        CubeFace cv cr = fromJust $ Map.lookup currentBlock blocksMap
        nFacesDir =
          map rotationHelper $
          filter (\(c, _) -> elem c blocks && notElem c nextSeq && Map.notMember c blocksMap) $
          map (\d -> (addCoord currentBlock (directions !! d), d)) [0 .. 3]

        rotationHelper (C r c, rot) = (C r c, assignment)
          where
            key = CubeFace cv (mod (cr + rot) numDirections)
            (CubeFace av ar) = fromJust (Map.lookup key cubeEdges)
            assignment = CubeFace av (mod (ar - rot + 2) numDirections)

        updatedBlocksMap = Map.union blocksMap (Map.fromList nFacesDir)
        updatedNextSeq = tail nextSeq ++ map fst nFacesDir


part2 :: GridMap -> Int
part2 gridMap = evaluate findNext gridMap
  where
    (GridMap grid numRows numCols blockSize _ _) = gridMap
    blockAssignments = foldCube gridMap
    invBlockAssignments = Map.fromList $ map (\(k, v) -> (value v, k)) $ Map.toList blockAssignments

    findNext (initCoord, dindex) = (nextCoord, nextDindex)
      where
        (C ir ic) = initCoord
        direction = directions !! dindex
        candCoord = C (ir + getX direction) (ic + getY direction)
        (C rc cc) = candCoord
        currentBlock = C (div ir blockSize) (div ic blockSize)
        candBlock = C (div rc blockSize) (div cc blockSize)

        exitEdge = rotateCubeFace (fromJust (Map.lookup currentBlock blockAssignments)) dindex
        nextEdge = fromJust (Map.lookup exitEdge cubeEdges)
        nextBlock = fromJust (Map.lookup (value nextEdge) invBlockAssignments)
        nextAssignment = fromJust (Map.lookup nextBlock blockAssignments)
        nextDindex
          | currentBlock == candBlock = dindex
          | otherwise                 = mod (rotation nextEdge - rotation nextAssignment + 2) numDirections
        blockCoord = C (ir - getX currentBlock * blockSize) (ic - getY currentBlock * blockSize)
        exitCoord
          | dindex == 0 = getX blockCoord
          | dindex == 1 = blockSize - 1 - getY blockCoord
          | dindex == 2 = blockSize - 1 - getX blockCoord
          | otherwise   = getY blockCoord
        enterCoord
          | nextDindex == 0 = C exitCoord 0
          | nextDindex == 1 = C 0 (blockSize - 1 - exitCoord)
          | nextDindex == 2 = C (blockSize - 1 - exitCoord) (blockSize - 1)
          | otherwise = C (blockSize - 1) exitCoord
        nextCoord
          | currentBlock == candBlock = candCoord
          | otherwise = C (getX enterCoord + getX nextBlock * blockSize) (getY enterCoord + getY nextBlock * blockSize)

main = do
  testInputString <- readFile "test_input.dat"
  let testGridMap = parse testInputString
  inputString <- readFile "input.dat"
  let gridMap = parse inputString
  print (part1 testGridMap)
  print (part1 gridMap)
  print (part2 testGridMap)
  print (part2 gridMap)
