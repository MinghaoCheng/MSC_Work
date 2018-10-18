module Main where
import HDL.Hydra.Core.Lib
import TrafficLight

main :: IO ()
main = run_traffic testdata

testdata :: [[Int]]
testdata =
    [[0], [0], [0], [0], [0], [0], [0], [0], [0], [0],
     [1], [0], [0], [0], [0], [0], [0], [0], [0], [0]]

run_counter :: [[Int]] -> IO ()
run_counter input = runAllInput input output
    where
        reset = getbit input 0
        out = counter reset
        output = [string "out = ", bits out]


run_traffic :: [[Int]] -> IO ()
run_traffic input = runAllInput input output
    where
        reset = getbit input 0
        (green, amber, red) = trafficLight reset
        output = [string " reset = ", bit reset,
                  string " green = ", bit green,
                  string " amber = ", bit amber,
                  string " red = ", bit red]
            