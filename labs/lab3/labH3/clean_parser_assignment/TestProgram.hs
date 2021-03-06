{- Test for Program -}
module TestProgram where

import Program
p, p1 :: Program.T
p = fromString  ("\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end")

p1 = fromString  ("\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\                    
\    p := p*10;\
\    n :=q;\
\  end\
\write s;")

sp = putStr (toString p)

rp = Program.exec p [3,16]

rp1 = Program.exec p1 [1024, 2]

pr :: Program.T
pr = fromString ("\
\read k;\
\write k;\
\repeat\
\ begin\
\ k := k + 1;\
\ write k;\
\ end\
\until k;")

spr = putStr (toString pr)
rpr k = Program.exec pr [k]

-- *TestProgram> spr
-- read k;
-- write k;
-- repeat
-- begin
-- k := k+1;
-- write k;
-- end
-- until k;
-- *TestProgram> rpr (-2)
-- [-2,-1,0,1]
-- *TestProgram> rpr (-1)
-- [-1,0,1]
-- *TestProgram> rpr 0
-- [0,1]
-- *TestProgram> rpr 1
-- [1,2]
-- *TestProgram> rpr 2
-- [2,3]
-- *TestProgram> 