module Main where

import Data.Bits (xor)
import Test.HUnit (Test(..), assertEqual, runTestTTAndExit)

data Registers =
  Registers
    { regA :: Int
    , regB :: Int
    , regC :: Int
    }
  deriving (Show, Eq)

data VMState =
  VMState
    { registers :: Registers
    , program :: [Int]
    , instructionPointer :: Int
    , output :: [Int]
    }
  deriving (Show, Eq)

step :: VMState -> VMState
step state = state'
  where
    r = registers state
    instr = program state !! instructionPointer state
    instructionPointer' = instructionPointer state + 2
    operand = program state !! (instructionPointer state + 1)
    combo
      | operand < 4 = operand
      | operand == 4 = regA r
      | operand == 5 = regB r
      | operand == 6 = regC r
      | otherwise = error "Invalid operand"
    state'
      | instr == 0 =    -- adv
        state
          { registers = r {regA = regA r `div` (2 ^ combo)}
          , instructionPointer = instructionPointer'
          }
      | instr == 1 =   -- bxl
        state
          { registers = r {regB = xor (regB r) operand}
          , instructionPointer = instructionPointer'
          }
      | instr == 2 =   -- bst
        state
          { registers = r {regB = mod combo 8}
          , instructionPointer = instructionPointer'
          }
      | instr == 3 =  -- jnz
        state
          { instructionPointer =
              if regA r == 0
                then instructionPointer'
                else operand
          }
      | instr == 4 =  -- bxc
        state
          { registers = r {regB = xor (regB r) (regC r)}
          , instructionPointer = instructionPointer'
          }
      | instr == 5 =  -- out
        state
          { output = output state ++ [combo `mod` 8]
          , instructionPointer = instructionPointer'
          }
      | instr == 6 =  -- bvd
        state
          { registers = r {regB = regA r `div` (2 ^ combo)}
          , instructionPointer = instructionPointer'
          }
      | instr == 7 =  -- cdv
        state
          { registers = r {regC = regA r `div` (2 ^ combo)}
          , instructionPointer = instructionPointer'
          }
      | otherwise = error "Invalid instruction"

runProgram :: VMState -> VMState
runProgram state
  | instructionPointer state >= length (program state) = state
  | otherwise = runProgram $ step state

-- Brute force solution. Too slow to work on the main input.
-- part2BruteForce :: VMState -> Int
part2BruteForce state = go 0
  where
    r = registers state
    go ra | output results == program state = ra
          | firstProg /= output results = go (ra + 1)
          | length (output results) > length (program state) = go (ra + 1)
          | otherwise = go (ra + 1)
      where
        s' = state {registers = r {regA = ra}}
        results = runProgram s'
        n = length (output results)
        firstProg = take n (program state)

-- An approach that uses the specifics of the main input.
-- The program is basically a loop that calculates a value from a.
-- Something close to
--    while a != 0:
--        b = ((a % 8) ^ 1) ^ (a // 2 ** ((a % 8) ^ 1)) ^ 6
--        print(b % 8)
--        a //= 8
part2 :: VMState -> Int
part2 state = go 0
  where
    p = program state
    go a
      | p == res = a
      | res == pEnd = go (a * 8)
      | otherwise = go (a + 1)
      where
        res =
          output $ runProgram state {registers = (registers state) {regA = a}}
        n = length res
        pEnd = reverse $ take n $ reverse p

-- Main entry point
main :: IO ()
main = do
  let mode = "run" -- Change this to "run" for normal execution
  if mode == "test"
    then runTests
    else runExample

-- Example usage
runExample :: IO ()
runExample = do
  let exampleVm = VMState (Registers 729 0 0) [0, 1, 5, 4, 3, 0] 0 []
  let mainVm =
        VMState
          (Registers 18427963 0 0)
          [2, 4, 1, 1, 7, 5, 0, 3, 4, 3, 1, 6, 5, 5, 3, 0]
          0
          []
  putStrLn $ "Example: " ++ show (runProgram exampleVm)
  putStrLn $ "Part 1: " ++ show (output $ runProgram mainVm)
  putStrLn $ "Part 2: " ++ show (part2 mainVm)

-- Tests
runTests :: IO ()
runTests =
  runTestTTAndExit $
  TestList
    [ TestCase $
      assertEqual
        "Empty program should output nothing"
        []
        (output $ runProgram (VMState (Registers 0 0 0) [] 0 []))
    , TestCase $
      assertEqual
        "Adv Operation"
        (VMState (Registers 1 0 0) [0, 2] 2 [])
        (step (VMState (Registers 5 0 0) [0, 2] 0 []))
    , TestCase $
      assertEqual
        "Bxl Operation"
        (VMState (Registers 0 6 0) [1, 3] 2 [])
        (step (VMState (Registers 0 5 0) [1, 3] 0 []))
    , TestCase $
      assertEqual
        "Bst Operation"
        (VMState (Registers 0 2 10) [2, 6] 2 [])
        (step (VMState (Registers 0 0 10) [2, 6] 0 []))
    , TestCase $
      assertEqual
        "Jnz Operation (A=0)"
        (VMState (Registers 0 2 10) [3, 6] 2 [])
        (step (VMState (Registers 0 2 10) [3, 6] 0 []))
    , TestCase $
      assertEqual
        "Jnz Operation (A/=0)"
        (VMState (Registers 1 2 10) [3, 6] 6 [])
        (step (VMState (Registers 1 2 10) [3, 6] 0 []))
    , TestCase $
      assertEqual
        "Bxc Operation"
        (VMState (Registers 0 6 3) [4, 0] 2 [])
        (step (VMState (Registers 0 5 3) [4, 0] 0 []))
    , TestCase $
      assertEqual
        "Out Operation"
        (VMState (Registers 11 5 3) [5, 4] 2 [3])
        (step (VMState (Registers 11 5 3) [5, 4] 0 []))
    , TestCase $
      assertEqual
        "Bdv Operation"
        (VMState (Registers 5 1 0) [6, 2] 2 [])
        (step (VMState (Registers 5 0 0) [6, 2] 0 []))
    , TestCase $
      assertEqual
        "Cdv Operation"
        (VMState (Registers 5 0 1) [7, 2] 2 [])
        (step (VMState (Registers 5 0 0) [7, 2] 0 []))
    , TestCase $
      assertEqual
        "Example 1"
        1
        (regB $ registers $ runProgram (VMState (Registers 1 2 9) [2, 6] 0 []))
    , TestCase $
      assertEqual
        "Example 2"
        [0, 1, 2]
        (output $
         runProgram (VMState (Registers 10 2 9) [5, 0, 5, 1, 5, 4] 0 []))
    , TestCase $
      assertEqual
        "Example 3"
        [4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0]
        (output $
         runProgram (VMState (Registers 2024 2 9) [0, 1, 5, 4, 3, 0] 0 []))
    , TestCase $
      assertEqual
        "Example 4"
        26
        (regB $ registers $ runProgram (VMState (Registers 0 29 9) [1, 7] 0 []))
    , TestCase $
      assertEqual
        "Example 5"
        44354
        (regB $
         registers $ runProgram (VMState (Registers 0 2024 43690) [4, 0] 0 []))
    ]
