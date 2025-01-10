import Data.List
import Data.List.Split (splitOn)

data Coord2D =
  Coord2D
    { x :: Int
    , y :: Int
    }
  deriving (Show, Eq)

data SensorBeaconPair =
  SensorBeaconPair
    { sensor :: Coord2D
    , beacon :: Coord2D
    }
  deriving (Show, Eq)

mHDistance :: Coord2D -> Coord2D -> Int
mHDistance (Coord2D x1 y1) (Coord2D x2 y2) = abs (x2 - x1) + abs (y2 - y1)

mHDistanceSB :: SensorBeaconPair -> Int
mHDistanceSB (SensorBeaconPair sensor beacon) = mHDistance sensor beacon

data Interval
  = Empty
  | Interval
      { start :: Int
      , stop :: Int
      }
  deriving (Show, Eq, Ord)

width :: Interval -> Int
width iv = stop iv - start iv

inInterval :: Interval -> Int -> Bool
inInterval iv x = start iv <= x && x < stop iv

inIntervals :: [Interval] -> Int -> Bool
inIntervals ivs x = any (`inInterval` x) ivs

removeIntervalOverlaps :: [Interval] -> [Interval]
removeIntervalOverlaps = helper . sort
  where
    helper :: [Interval] -> [Interval]
    helper (iv1:iv2:ivs) =
      if stop iv1 < start iv2
        then iv1 : helper (iv2 : ivs)
        else helper (Interval (start iv1) (max (stop iv1) (stop iv2)) : ivs)
    helper ivs = ivs

parseInput :: String -> [SensorBeaconPair]
parseInput s = sensorBeaconPairs
  where
    ls = map (words . filter (`notElem` ",:")) (lines s)
    sensors = map (\l -> Coord2D (read (drop 2 (l !! 2))) (read (drop 2 (l !! 3)))) ls
    beacons = map (\l -> Coord2D (read (drop 2 (l !! 8))) (read (drop 2 (l !! 9)))) ls
    sensorBeaconPairs = zipWith SensorBeaconPair sensors beacons

part1 :: Int -> [SensorBeaconPair] -> Int
part1 yInd sblist = numBlocked - sensorsInIntervals
  where
    relevantBeacons = filter (\sb -> mHDistanceSB sb >= abs (yInd - y (sensor sb))) sblist
    beaconIntervalHelper sb = Interval start stop
      where
        half_width = mHDistanceSB sb - abs (yInd - y (sensor sb))
        start = x (sensor sb) - half_width
        stop = x (sensor sb) + half_width + 1
    beaconIntervals = removeIntervalOverlaps $ map beaconIntervalHelper relevantBeacons
    sensorsInIntervals =
      length $ nub $ filter (\sb -> y sb == yInd && inIntervals beaconIntervals (x sb)) $ map beacon sblist
    numBlocked = sum $ map width beaconIntervals

beaconEdgePoints :: SensorBeaconPair -> [Coord2D]
beaconEdgePoints sb = r
  where
    mhd = 1 + mHDistanceSB sb
    (Coord2D sx sy) = sensor sb
    r =
      concatMap
        (\i ->
           [ Coord2D (sx + mhd - i) (sy + i)
           , Coord2D (sx + mhd - i) (sy - i)
           , Coord2D (sx - mhd + i) (sy + i)
           , Coord2D (sx - mhd + i) (sy - i)
           ])
        [0 .. mhd]

part2 :: Int -> [SensorBeaconPair] -> Int
part2 yInd sblist = x * 4000000 + y
  where
    edgePoints =
      filter (\(Coord2D x y) -> 0 <= x && x <= yInd && 0 <= y && y <= yInd) $ concatMap beaconEdgePoints sblist
    possibleCoord c =  all (\sb -> mHDistance c (sensor sb) > mHDistanceSB sb) sblist
    finalPoints = filter possibleCoord edgePoints
    (Coord2D x y) = head finalPoints

main = do
  testInput <- readFile "test_input.dat"
  let parsedTestInput = parseInput testInput
  print (part1 10 parsedTestInput)
  input <- readFile "input.dat"
  let parsedInput = parseInput input
  print (part1 2000000 parsedInput)
  print (part2 20 parsedTestInput)
  print (part2 4000000 parsedInput)
