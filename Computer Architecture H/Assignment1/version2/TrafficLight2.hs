module TrafficLight2 where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

rippleAdder4 :: Bit b => b -> [b] -> [b] -> (b, [b])
rippleAdder4 cin [x0, x1, x2, x3] [y0, y1, y2, y3] = (c0, [s0, s1, s2, s3])
    where
        (c0, s0) = fullAdd (x0, y0) c1
        (c1, s1) = fullAdd (x1, y1) c2
        (c2, s2) = fullAdd (x2, y2) c3
        (c3, s3) = fullAdd (x3, y3) cin

incrementor4 :: CBit b => b -> [b] -> [b]
incrementor4 reset [th0, th1, th2, th3] = [out0, out1, out2, out3]
    where
        (cout, [s0, s1, s2, s3]) = rippleAdder4 one [out0, out1, out2, out3] [zero, zero, zero, zero]
        out0 = dff(mux1 reset_internal s0 zero)
        out1 = dff(mux1 reset_internal s1 zero)
        out2 = dff(mux1 reset_internal s2 zero)
        out3 = dff(mux1 reset_internal s3 zero)
        reset_internal = or2 reset (and4 (xnor2 th0 out0) (xnor2 th1 out1) (xnor2 th2 out2) (xnor2 th3 out3))

trafficLight :: CBit b => b -> b -> (b, b, b, b, b)
trafficLight reset requeset = (green, amber, red, wait, walk)
    where
        green = mux1 reset state0 zero
        amber = mux1 reset (or2 state1 state3) zero
        red = mux1 reset state2 zero
        wait = or2 green amber
        walk = red

        th0 = and4 (inv s0) (inv s1) (inv s2) (inv s3)      --0000
        th1 = and4 (inv s0) (inv s1) s2 s3                  --0011
        th2 = and4 (inv s0) s1 (inv s2) (inv s3)            --0100
        th3 = and4 s0 (inv s1) (inv s2) (inv s3)            --1000

        state0 = reg1 (or2 th0 th1)
                        (and2 th0 (inv reset))
        state1 = reg1 (or2 th1 th2)
                        th1
        state2 = reg1 (or2 th2 th3)
                        th2
        state3 = reg1 (or2 th3 th0)
                        th3
        
        reset_incrementor = reg1 (or3 reset requeset th3) (or2 reset th3)

        [s0, s1, s2, s3] = incrementor4 reset_incrementor [one, zero, zero, zero]