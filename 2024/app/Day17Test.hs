module Main where

import Test.HUnit
import MyLib (Registers(..), VMState(..), runProgram)

-- Test: Empty program does nothing
testEmptyProgram = TestCase $ assertEqual
  "An empty program should output nothing"
  []
  (runProgram (Registers 0 0 0) [])

main :: IO ()
main = runTestTTAndExit $ TestList [testEmptyProgram]
