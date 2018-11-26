-----------------------------------------------------------------------
--  ArrayMax: machine language program for the Sigma16 architecture
-----------------------------------------------------------------------

{- A machine language program for the Sigma16 architecture that
searches an array of natural numbers for the maximal element.  The
loop terminates when a negative element is encountered. -}



-- this program is manipulated using loadxi instruction
module Main where
   import M1run
   
   main :: IO ()
   main = run_Sigma16_program loadxiRun 10000
   
   -----------------------------------------------------------------------
   
   loadxiRun :: [String]
   loadxiRun =
   -- Machine Language  Addr    Assembly Language     Comment
   -- ~~~~~~~~~~~~~~~~  ~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    [
                     -- 0000 ; ArrayMax: find the maximum element of an array
                     -- 0000
                     -- 0000 ; The program is given
                     -- 0000 ;   *  a natural number n, assume n>0
                     -- 0000 ;   *  an n-element array x[0], x[1], ..., x[n-1]
                     -- 0000 ;  It calculates
                     -- 0000 ;   * max = the maximum element of x
                     -- 0000
                     -- 0000 ; Since n>0, the array x contains at least one
                     -- 0000 ;  element, and a maximum element is guaranteed
                     -- 0000 ;  to exist.
                     -- 0000
                     -- 0000 ; program ArrayMax
                     -- 0000 ;   max := x[0]
                     -- 0000 ;   for i := 1 to n-1 step 1
                     -- 0000 ;       if x[i] > max
                     -- 0000 ;         then max := x[i]
                     -- 0000
                     -- 0000 ; Register usage
                     -- 0000 ;   R1 = i
                     -- 0000 ;   R2 = n
                     -- 0000 ;   R3 = max
                     -- 0000 ;   R4 = x[i]
                     -- 0000 ;   R5 = i < n
                     -- 0000 ;   R6 = x[i] > max
                     -- 0000
                     -- 0000 ; Initialise
                     -- 0000
     "f201", "0014", -- 0000 start load R2,n[R0]
     "f100", "0000", -- 0002       lea   R1,1[R0]      ; R1 = 0
     "f317", "0016", -- 0004       loadxi R3,x[R1]     ; R3 = i; i++
     "4512",         -- 0006       cmplt R5,R1,R2      ; R5 = (i<n)
     "f504", "0011", -- 0007       jumpf R5,done[R0]   ; if i>=n then goto done
                     -- 0009
                     -- 0009 ; if x[i] > max
                     -- 0009
     "f417", "0016", -- 0009       loadxi  R4,x[R1]      ; R4 = i, i++
     "6643",         -- 000b       cmpgt R6,R5,R3      ; R6 = (x[i] > max)
     "f604", "000f", -- 000c       jumpf R6,next[R0]   ; if x[i]<=max, goto neg
                     -- 000e             
                     -- 000e ; then max := x[i]
                     -- 000e             
     "0340",         -- 000e       add   R3,R4,R0      ; max := x[i]
                     -- 000f ; Bottom of loop
                     -- 000f
     "f003", "0006", -- 000f        jump  loop[R0]     ; go to top of loop
                     -- 0011             
                     -- 0011 ; Exit from loop

     "f302", "0015", -- 0011 done   store R3,max[R0]   ; max = R3
     "d000",         -- 0013        trap  R0,R0,R0     ; terminate
                
                     -- 0014 ; Data area
     "0006",         -- 0014 n        data   6
     "0000",         -- 0015 max      data   0
     "0012",         -- 0016 x        data  18
     "0003",         -- 0017          data   3
     "0015",         -- 0018          data  21
     "fffe",         -- 0019          data  -2
     "0028",         -- 001a          data  40
     "0019"          -- 001b          data  25
    ]
   
   -----------------------------------------------------------------------
   