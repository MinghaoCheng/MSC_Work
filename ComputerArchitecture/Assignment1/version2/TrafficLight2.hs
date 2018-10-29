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


counter16 :: CBit b => b -> b -> [b]
counter16 reset cin = [out0, out1, out2, out3, out4, out5, out6, out7, out8, out9, out10, out11, out12, out13, out14, out15]
    where
        out0 = dff(mux1 reset s0 zero)
        out1 = dff(mux1 reset s1 zero)
        out2 = dff(mux1 reset s2 zero)
        out3 = dff(mux1 reset s3 zero)
        out4 = dff(mux1 reset s4 zero)
        out5 = dff(mux1 reset s5 zero)
        out6 = dff(mux1 reset s6 zero)
        out7 = dff(mux1 reset s7 zero)
        out8 = dff(mux1 reset s8 zero)
        out9 = dff(mux1 reset s9 zero)
        out10 = dff(mux1 reset s10 zero)
        out11 = dff(mux1 reset s11 zero)
        out12 = dff(mux1 reset s12 zero)
        out13 = dff(mux1 reset s13 zero)
        out14 = dff(mux1 reset s14 zero)
        out15 = dff(mux1 reset s15 zero)

        (cout, [s0, s1, s2, s3]) = rippleAdder4 c3 [out0, out1, out2, out3] [zero, zero, zero, zero]
        (c3, [s4, s5, s6, s7]) = rippleAdder4 c2 [out4, out5, out6, out7] [zero, zero, zero, zero]
        (c2, [s8, s9, s10, s11]) = rippleAdder4 c1 [out8, out9, out10, out11] [zero, zero, zero, zero]
        (c1, [s12, s13, s14, s15]) = rippleAdder4 cin [out12, out13, out14, out15] [zero, zero, zero, zero]

trafficLight :: CBit b => b -> b -> (b, b, b, b, b, [b])
trafficLight reset requeset = (green, amber, red, wait, walk, [c15, c14, c13, c12, c11, c10, c9, c8, c7, c6, c5, c4, c3, c2, c1, c0])
    where
        green = mux1 reset state0 zero
        amber = mux1 reset (or2 state1 state3) zero
        red = mux1 reset state2 zero
        wait = or2 green amber
        walk = red

        th0 = and4 (inv s0) (inv s1) (inv s2) (inv s3)      --0000
        th1 = and4 (inv s0) (inv s1) s2 (inv s3)            --0010
        th2 = and4 (inv s0) (inv s1) s2 s3                  --0011
        th3 = and4 (inv s0) s1 s2 s3                        --0111
        th4 = and4 s0 (inv s1) (inv s2) (inv s3)            --1000

        state0 = reg1 (or3 th4 th1 reset)
                        (or2 th0 th4)
        state1 = reg1 (or3 th1 th2 reset)
                        th1
        state2 = reg1 (or3 th2 th3 reset)
                        th2
        state3 = reg1 (or3 th3 th4 reset)
                        th3
        
        reset_incrementor = reg1 (or3 reset requeset th3) (or2 reset th3)

        [c15, c14, c13, c12, c11, c10, c9, c8, c7, c6, c5, c4, c3, c2, c1, c0] = counter16 reset requeset

        [s0, s1, s2, s3] = incrementor4 reset_incrementor [one, zero, zero, zero]
