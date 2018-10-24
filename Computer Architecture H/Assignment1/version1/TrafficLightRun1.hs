module Main where
import HDL.Hydra.Core.Lib
import TrafficLight1

main :: IO ()
main = run_traffic testdata

testdata :: [[Int]]
testdata =
    [[1], [0], [0], [0], [0], [0], [0], [0], [0], [0],
     [0], [0], [0], [0], [0], [0], [0], [0], [0], [0],
     [0], [0], [0], [0], [0], [0], [0], [0], [0], [0]]

run_traffic :: [[Int]] -> IO ()
run_traffic input = runAllInput input output
    where
        reset = getbit input 0
        (green, amber, red) = trafficLight reset
        output = [string " reset = ", bit reset,
                  string " green = ", bit green,
                  string " amber = ", bit amber,
                  string " red = ", bit red]