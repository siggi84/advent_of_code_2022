import Control.Monad (guard)
import Data.Foldable (foldl')
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String (Parser)

data Instruction
  = DO
  | DONT
  | Mul Int
  deriving (Show)

number :: Parser Int
number =
  read <$> do
    digits <- many1 digit
    guard (length digits <= 3)
    return digits

parseMul :: Parser Instruction
parseMul = do
  try $ string "mul"
  char '('
  x <- number
  char ','
  y <- number
  char ')'
  return $ Mul (x * y)

parseDont :: Parser Instruction
parseDont = DONT <$ try (string "don't")

parseDo :: Parser Instruction
parseDo = DO <$ try (string "do")

parseStep :: Parser (Maybe Instruction)
parseStep =
  (Just <$> try parseMul)  <|> 
  (Just <$> try parseDont) <|>
  (Just <$> try parseDo)   <|>
  (anyChar >> return Nothing) -- On failure, consume one char

parseAll :: Parser [Instruction]
parseAll = catMaybes <$> many parseStep

part1 :: [Instruction] -> Int
part1 = foldl' (\acc i -> case i of Mul n -> n + acc; _ -> acc) 0

part2 :: [Instruction] -> Int
part2 instructions = snd $ foldl' step (True, 0) instructions
  where
    step (state, acc) instr =
      case instr of
        Mul n -> (state, if state then n + acc else acc)
        DO -> (True, acc)
        DONT -> (False, acc)

main :: IO ()
main = do
  input <- readFile "data/day3.txt"
  case parse parseAll "" input of
    Left err -> print err
    Right instructions -> do
      print $ part1 instructions
      print $ part2 instructions
