module Incrementor where
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

incrementor :: CBit b => b -> [b]
incrementor reset = [out0, out1, out2, out3]
    where
        out0 = dff (mux1 reset y0 zero)
        out1 = dff (mux1 reset y1 zero)
        out2 = dff (mux1 reset y2 zero)
        out3 = dff (mux1 reset y3 zero)
        (c0 , y0 ) = halfAdd out0 c1
        (c1 , y1 ) = halfAdd out1 c2
        (c2 , y2 ) = halfAdd out2 c3
        (c3 , y3 ) = halfAdd out3 one

incrementor16 :: CBit b => b -> [b]
incrementor16 reset = [out0, out1, out2, out3, out4, out5, out6, out7, out8, out9, out10, out11, out12, out13, out14, out15]
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
        (c1, [s12, s13, s14, s15]) = rippleAdder4 one [out12, out13, out14, out15] [zero, zero, zero, zero]
