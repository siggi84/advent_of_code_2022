import Control.Monad (forM_)
import Data.Either (rights)
import Data.Ix (inRange)
import Data.List (elemIndex, maximumBy, minimumBy)
import Data.Ord (comparing)
import Text.Parsec
import Text.Parsec.String (Parser)

data Robot =
  Robot
    { locationX :: Integer
    , locationY :: Integer
    , velocityX :: Integer
    , velocityY :: Integer
    }
  deriving (Show, Eq)

int :: Parser Integer
int = do
  sign <- optionMaybe (char '-')
  digits <- many1 digit
  let val = read digits
  pure $
    case sign of
      Just _ -> -val
      Nothing -> val

parseRobots :: String -> [Robot]
parseRobots input =
  let linesOfInput = lines input
      parsedLines = map (parse robotParser "") linesOfInput
   in rights parsedLines

robotParser :: Parser Robot
robotParser = do
  _ <- string "p="
  x <- int
  _ <- char ','
  y <- int
  spaces
  _ <- string "v="
  vx <- int
  _ <- char ','
  vy <- int
  eof
  return (Robot x y vx vy)

evolveRobot rows columns steps robot = robot'
  where
    x = (locationX robot + steps * velocityX robot) `mod` columns
    y = (locationY robot + steps * velocityY robot) `mod` rows
    robot' = robot {locationX = x, locationY = y}

evolveRobots rows columns steps = map (evolveRobot rows columns steps)

-- part1 :: Int -> Int -> [Robot] -> Int
part1 rows columns robots = quadMul
  where
    robots' = map (evolveRobot rows columns 100) robots
    locs = map (\r -> (locationX r, locationY r)) robots'
    cc = columns `div` 2
    rr = rows `div` 2
    q1 = ((0, 0), (cc - 1, rr - 1))
    q2 = ((cc + 1, 0), (columns - 1, rr - 1))
    q3 = ((0, rr + 1), (cc - 1, rows - 1))
    q4 = ((cc + 1, rr + 1), (columns - 1, rows - 1))
    quarters = [q1, q2, q3, q4]
    countInQuarter q = sum $ map (fromEnum . inRange q) locs
    quadMul = product $ map countInQuarter quarters

calcVariance robots = varianceX + varianceY
  where
    meanX = sum (map locationX robots) `div` toInteger (length robots)
    varianceX = sum $ map (\r -> (locationX r - meanX) ^ 2) robots
    meanY = sum (map locationY robots) `div` toInteger (length robots)
    varianceY = sum $ map (\r -> (locationY r - meanY) ^ 2) robots

part2 rows columns robots = elemIndex minVariance varianceValues
  where
    cc = columns `div` 2
    xVariance rs = sum $ map (\r -> (locationX r - cc) ^ 2) rs
    evolutions =
      robots :
      takeWhile
        (/= robots)
        (map (\i -> evolveRobots rows columns i robots) [1 ..])
    varianceValues = map calcVariance evolutions
    minVariance = minimum varianceValues

printRobots :: [Robot] -> IO ()
printRobots rs = do
  let xs = map locationX rs
      ys = map locationY rs
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
      robotSet = [(x, y) | Robot x y _ _ <- rs]
  forM_ [minY .. maxY] $ \y -> do
    forM_ [minX .. maxX] $ \x -> do
      if (x, y) `elem` robotSet
        then putStr "#"
        else putStr "."
    putStrLn ""

main :: IO ()
main = do
  -- inputTest <- readFile "data/day14_test.txt"
  -- print $ part1 7 11 $ parseRobots inputTest
  input <- readFile "data/day14.txt"
  print $ part1 103 101 $ parseRobots input
  print $ part2 103 101 $ parseRobots input
  printRobots $ evolveRobots 103 101 7569 $ parseRobots input
