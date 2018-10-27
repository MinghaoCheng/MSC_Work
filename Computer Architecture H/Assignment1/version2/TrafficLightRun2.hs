module Main where
import HDL.Hydra.Core.Lib
import TrafficLight2

main :: IO ()
main = run_traffic testdata

testdata :: [[Int]]
testdata =
    [[1,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,1],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],
    [0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,1],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],
    [0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]]

run_traffic :: [[Int]] -> IO ()
run_traffic input = runAllInput input output
    where
        reset = getbit input 0
        requeset = getbit input 1
        (green, amber, red, wait, walk, s) = 
            trafficLight reset requeset
        output = [string " Input: reset = ", bit reset,
                  string " request = ", bit requeset,
                  string " Output: green = ", bit green,
                  string " amber = ", bit amber,
                  string " red = ", bit red,
                  string " wait = ", bit wait,
                  string " walk = ", bit walk,
                  string " RequestCounter = ", bindec 16 s]
                