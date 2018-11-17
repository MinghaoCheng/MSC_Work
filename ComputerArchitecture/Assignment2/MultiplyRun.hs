------------------------------------------------------------------------
-- Multiplier simulation driver
------------------------------------------------------------------------

module Main where
import HDL.Hydra.Core.Lib
import Multiply

------------------------------------------------------------------------
-- Main program to run multiplier on test data

main :: IO ()
main =
  do run_multiply mult_test_data_1

------------------------------------------------------------------------
-- Test data

mult_test_data_1 :: [[Int]]
mult_test_data_1 =
--     start  x    y
--     ~~~~~~~~~~~~~~
       [[1,  50,  75],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [1,  25,  25],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [1,   2,   3],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0],
        [0,   0,   0]]

------------------------------------------------------------------------
-- Simulation driver

run_multiply :: [[Int]] -> IO ()
run_multiply input = runAllInput input output
  where
-- Size parameter
    k = 16

-- Extract input signals from readable input
    start = getbit   input 0
    x     = getbin k input 1
    y     = getbin k input 2

-- Connect the circuit to its inputs and outputs
    (rdy,prod) = multiply k start x y

-- Format the signals for output
    output =
      [string "Input: ",
       bit start, bindec 16 x, bindec 16 y,
       string "  Output: ",
       bit rdy, bindec 32 prod]
