module Design.RCA where

import Cell.Cell
import Cell.Phase
import DesignTools

cin , a0 , b0 , a1 , b1 , sum0 , sum1 , cout  :: Cell
cin   = inputCell  { label = "Cin"  , loc = ( 0,  3, 0)                                , propagate = True }
a0    = inputCell  { label = "A"    , loc = (-3,  0, 0) , number = 0                                      }
b0    = inputCell  { label = "B"    , loc = ( 0, -3, 0) , number = 0                                      }
a1    = inputCell  { label = "A"    , loc = (-3,  0, 4) , number = 1 , phase = clock 1                    }
b1    = inputCell  { label = "B"    , loc = ( 0, -3, 4) , number = 1 , phase = clock 1                    }
sum0  = outputCell { label = "Sum"  , loc = ( 0,  0, 2) , number = 0 , phase = clock 1                    }
sum1  = outputCell { label = "Sum"  , loc = ( 0,  0, 6) , number = 1 , phase = clock 2                    }
cout  = outputCell { label = "Cout" , loc = ( 0,  3, 4) , number = 1 , phase = clock 2 , propagate = True }

sA1, sA2, sA3 :: Cell
sA1 = clock0Cell { loc = ( 0, 2, 0) }
sA2 = clock0Cell { loc = ( 0,-2, 0) }
sA3 = clock0Cell { loc = (-2, 0, 0) }
tA1, tA2, tA3 :: Cell
tA1 = clock0Cell { loc = (-2, 0, 1) }
tA2 = clock0Cell { loc = ( 0, 3, 1) }
tA3 = clock0Cell { loc = ( 0,-3, 1) }
uA1, uA2, uA3, uA4, uA5, uA6, uA7 :: Cell
uA1 = clock0Cell { loc = (-2, 0, 2) }
uA2 = clock0Cell { loc = ( 0, 3, 2) }
uA3 = clock0Cell { loc = ( 0,-3, 2) }
uA4 = clock0Cell { loc = ( 0, 2, 2) }
uA5 = clock0Cell { loc = ( 0,-2, 2) }
uA6 = clock0Cell { loc = (-1, 1, 2) }
uA7 = clock0Cell { loc = (-1,-1, 2) }
pA1, pA2, pA3, pA4, pA5, pA6 :: Cell
pA1 = clock1Cell { loc = (-1, 0, 0) }
pA2 = clock1Cell { loc = ( 0, 0, 0) }
pA3 = clock1Cell { loc = ( 0, 1, 0) }
pA4 = clock1Cell { loc = ( 0,-1, 0) }
pA5 = clock1Cell { loc = ( 1, 0, 0) }
pA6 = clock1Cell { loc = ( 2, 0, 0) }
rA1, rA2, rA3 :: Cell
rA1 = clock1Cell { loc = ( 0, 1, 2) }
rA2 = clock1Cell { loc = (-1, 0, 2) }
rA3 = clock1Cell { loc = ( 0,-1, 2) }
wA1, wA2, wA3, wA4, wA5 :: Cell
wA1 = clock1Cell { loc = ( 3, 0, 0) }
wA2 = clock1Cell { loc = ( 3, 0, 1) }
wA3 = clock1Cell { loc = ( 3, 0, 2) }
wA4 = clock1Cell { loc = ( 3, 0, 3) }
wA5 = clock1Cell { loc = ( 3, 0, 4) }
sB1, sB2, sB3 :: Cell
sB1 = clock1Cell { loc = ( 2, 0, 4) }
sB2 = clock1Cell { loc = ( 0,-2, 4) }
sB3 = clock1Cell { loc = (-2, 0, 4) }
tB1, tB2, tB3 :: Cell
tB1 = clock1Cell { loc = ( 0,-2, 5) }
tB2 = clock1Cell { loc = ( 3, 0, 5) }
tB3 = clock1Cell { loc = (-3, 0, 5) }
uB1, uB2, uB3, uB4, uB5, uB6, uB7 :: Cell
uB1 = clock1Cell { loc = ( 0,-2, 6) }
uB2 = clock1Cell { loc = ( 3, 0, 6) }
uB3 = clock1Cell { loc = (-3, 0, 6) }
uB4 = clock1Cell { loc = ( 2, 0, 6) }
uB5 = clock1Cell { loc = (-2, 0, 6) }
uB6 = clock1Cell { loc = ( 1,-1, 6) }
uB7 = clock1Cell { loc = (-1,-1, 6) }
pB1, pB2, pB3, pB4, pB5, pB6 :: Cell
pB1 = clock2Cell { loc = (-1, 0, 4) }
pB2 = clock2Cell { loc = ( 0, 0, 4) }
pB3 = clock2Cell { loc = ( 0, 1, 4) }
pB4 = clock2Cell { loc = ( 0,-1, 4) }
pB5 = clock2Cell { loc = ( 1, 0, 4) }
pB6 = clock2Cell { loc = ( 0, 2, 4) }
rB1, rB2, rB3 :: Cell
rB1 = clock2Cell { loc = ( 1, 0, 6) }
rB2 = clock2Cell { loc = ( 0,-1, 6) }
rB3 = clock2Cell { loc = (-1, 0, 6) }
wB1, wB2, wB3, wB4 :: Cell
wB1 = clock2Cell { loc = ( 0, 3, 5) }
wB2 = clock2Cell { loc = ( 0, 3, 6) }
wB3 = clock2Cell { loc = ( 0, 3, 7) }
wB4 = clock2Cell { loc = ( 0, 3, 8) }

rcaCellEnv :: [Cell]
rcaCellEnv = [a0,b0,cin,sum0,sA1,sA2,sA3,tA1,tA2,tA3,uA1,uA2,uA3,uA4,uA5,uA6,uA7,pA1,pA2,pA3,pA4,pA5,pA6,rA1,rA2,rA3,wA1,wA2,wA3,wA4,wA5,
           a1,b1,cout,sum1,sB1,sB2,sB3,tB1,tB2,tB3,uB1,uB2,uB3,uB4,uB5,uB6,uB7,pB1,pB2,pB3,pB4,pB5,pB6,rB1,rB2,rB3,wB1,wB2,wB3,wB4]