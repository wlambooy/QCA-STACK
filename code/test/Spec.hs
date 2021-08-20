import BistableEngine
import FormalQCA

three_majI1 = inputCell {loc = (0,1,0)}
three_majI2 = inputCell {loc = (-1,0,0)}
three_majI3 = inputCell {loc = (0,-1,0)}
three_maj0 = defaultCell {loc = (0,0,0)}
three_maj1 = defaultCell {loc = (1,0,0)}
three_maj2 = defaultCell {loc = (2,0,0)}
three_majO = outputCell {loc = (3,0,0)}
cellenv = [three_majI1, three_majI2, three_majI3, three_maj0, three_maj1, three_maj2, three_majO]


-- QUICKTEST

ans = "[([\n\
\       (3.0,0.0,0.0):\t3.910693557744063e-3\n\
\       ],True),([\n\
\       (3.0,0.0,0.0):\t1.1467436742852105e-3\n\
\       ],True),([\n\
\       (3.0,0.0,0.0):\t1.6172062072378089e-3\n\
\       ],True),([\n\
\       (3.0,0.0,0.0):\t-1.1467436772998095e-3\n\
\       ],True),([\n\
\       (3.0,0.0,0.0):\t1.14674367507338e-3\n\
\       ],True),([\n\
\       (3.0,0.0,0.0):\t-1.6172062052478907e-3\n\
\       ],True),([\n\
\       (3.0,0.0,0.0):\t-1.1467436764057552e-3\n\
\       ],True),([\n\
\       (3.0,0.0,0.0):\t-3.910693556832911e-3\n\
\       ],True)]"
main :: IO ()
main = putStrLn "Test suite not yet implemented"
