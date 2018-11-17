module Main where
    import M1run
    
    main :: IO ()
    main = run_Sigma16_program arraymax 10000
    
    -----------------------------------------------------------------------
    
    arraymax :: [String]
    arraymax =
     [
      "f100", "0001", -- 0000 start lea   R1,1[R0]      ; R1 = constant 1
      "f207", "0005", -- 0002
      "d000",         -- 0004        trap  R0,R0,R0     ; terminate
                      
                      -- 0004 ; Data area
      "0006"          -- 0005 n        data   6
     ]
    
    -----------------------------------------------------------------------
    