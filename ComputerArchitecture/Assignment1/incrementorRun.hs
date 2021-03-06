module Main where
import HDL.Hydra.Core.Lib
import Incrementor

main :: IO ()
main = run_incrementor16 testdata

testdata :: [[Int]]
testdata =
    [[1], [0], [0], [0], [0], [0], [0], [0], [0], [0],
    [0], [0], [0], [0], [0], [0], [0], [0], [0], [0],
    [0], [0], [0], [0], [0], [0], [0], [0], [0], [0],
    [0], [0], [0], [0], [0], [0], [0], [0], [0], [0],
    [1], [0], [0], [0], [0], [0], [0], [0], [0], [0],
    [0], [0], [0], [0], [0], [0], [0], [0], [0], [0]]

run_incrementor :: [[Int]] -> IO ()
run_incrementor input = runAllInput input output
    where
        reset = getbit input 0
        s = incrementor reset
        output = [string " reset = ", bit reset,
                  string ", s = ", bits s]

run_incrementor16 :: [[Int]] -> IO ()
run_incrementor16 input = runAllInput input output
    where
        reset = getbit input 0
        s = incrementor16 reset
        output = [string " reset = ", bit reset,
                  string ", s = ", bindec 16 s]
