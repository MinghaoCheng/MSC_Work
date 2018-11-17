start:
    lea   R1,1[R0]
    loadxi R2,n[R0]
    trap  R0,R0,R0          ; terminate

n        data   6