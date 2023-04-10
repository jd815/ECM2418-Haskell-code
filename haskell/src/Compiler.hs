module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (Plus a1 a2) = acomp a1 ++ acomp a2 ++ [ADD]
acomp (N x) = [LOADI x]
acomp (V x) = [LOAD x]

--TODO Task 3.2
get :: BExp -> Bool
get (Bc b) = b

bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc b1)(b2)(n) =
  if (b1 == b2) then [JMP n]
  else []
bcomp (Not b1)(b2)(n) =
  if ((get b1) == b2) then []
  else [JMP n]
bcomp (And b1 b2)(b3)(n) = do
  let x= bcomp b2 b3 n
      y= bcomp b1 b3 n
  if ((x==[])&&(b3 == True)) then []
  else if ((y==[])&&(x/=[])&&(b3 == True)) then [JMP (length x)] ++ x
  else y ++ x
bcomp (Less a1 a2)(b)(n) =
  if(b==True) then acomp a1 ++ acomp a2 ++ [JMPLESS n]
  else acomp a1 ++ acomp a2 ++ [JMPGE n]

--TODO Task 3.3

ccomp :: Com -> [Instr]
ccomp (Assign v a) = acomp a ++ [STORE v]
ccomp (Seq c1 c2) = ccomp c1 ++ ccomp c2
ccomp (If b1 c1 c2) = bcomp b1 False (length (ccomp c1) + 1) ++ ccomp c1 ++ [JMP (length (ccomp c2))] ++ ccomp c2
ccomp (While b1 c1) = bcomp b1 False (length (ccomp c1)+1) ++ ccomp c1 ++ [JMP (-length (bcomp b1 True (3)) - length (ccomp c1) - 1)]
ccomp (SKIP) = [JMP 1]
