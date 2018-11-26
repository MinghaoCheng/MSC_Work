module Main where
    import M1run
    
    main :: IO ()
    main = run_Sigma16_program arraymax 10000
    
    -----------------------------------------------------------------------
    
    arraymax :: [String]
    arraymax =
     [
        "f207", "0005", -- 0000 loadxi R2,5[R0]
        "f307", "0006", -- 0002 loadxi R3,6[R0]
        "d000",         -- 0004        trap  R0,R0,R0     ; terminate
                      
                      -- ; Data area
        "0006",         -- 0005 n        data   6
        "000a"          -- 0006 n        data   a
     ]
    
    -----------------------------------------------------------------------
    