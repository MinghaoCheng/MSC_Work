module Main where
    import M1run
    
    main :: IO ()
    main = run_Sigma16_program mulRun 10000
    
    -----------------------------------------------------------------------
    
    mulRun :: [String]
    mulRun =
     [
      "f101", "000b", -- 0000 start load R1,x1[R0]
      "f201", "000c", -- 0002       load R2,y1[R0]
      "2312",         -- 0004       mul R3,R1,R2
      "f101", "000d", -- 0005 start load R1,x2[R0]
      "f201", "000e", -- 0006       load R2,y2[R0]
      "2312",         -- 0007       mul R3,R1,R2
      "d000",         -- 000a        trap  R0,R0,R0     ; terminate
                      
                      -- Data area
      "0002",         -- 000b x1
      "0003",         -- 000c y1
      "0025",         -- 000d x2
      "0025"          -- 000e y2
     ]
    
    -----------------------------------------------------------------------
    