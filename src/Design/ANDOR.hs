module Design.ANDOR where

import DesignTools
import Cell.Cell
import Cell.Phase

i0 , i1 , o :: Cell
i0 = inputCell  { label = "I"   , loc = ( -1 , 0 , 0 ) , number = 0                                      }
i1 = inputCell  { label = "I"   , loc = ( -1 , 0 , 2 ) , number = 1 , phase = clock 1                    }
o  = outputCell { label = "OUT" , loc = (  0 , 0 , 2 )              , phase = clock 2 , propagate = True }

a1 , a2 , b1 :: Cell
a1 = clock1Cell   { loc = (0,0,0) }
a2 = clock1Cell   { loc = (0,0,1) }
b1 = clock2Cell   { loc = (0,0,3) }

p1 , p2 , q1 , q2 :: Cell
p1 = negativeCell { loc = (1,0,2) }
p2 = negativeCell { loc = (1,0,4) }
q1 = positiveCell { loc = (1,0,2) }
q2 = positiveCell { loc = (1,0,4) }

andCellEnv :: [Cell]
andCellEnv = [i0,i1,o,a1,a2,b1,p1,p2]

orCellEnv :: [Cell]
orCellEnv = [i0,i1,o,a1,a2,b1,q1,q2]
