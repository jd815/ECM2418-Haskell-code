module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Machine

--TODO Task 2.1
data AExp = N Val | V Vname | Plus (AExp) (AExp)
    deriving (Eq, Read, Show)

--TODO Task 2.2
aval :: AExp -> State -> Val
aval (Plus a1 a2) (memory) = aval a1 memory + aval a2 memory
aval (N x)(memory) = x
aval (V x)(memory) = memory ! x

--TODO Task 2.1
data BExp = Bc Bool | Not BExp | And BExp BExp | Less AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool
bval (Bc b)(memory) = b
bval (Not b)(memory) = not (bval b memory)
bval (And b1 b2)(memory) =
  if ((bval b1 memory) == (bval b2 memory)) then bval b1 memory
  else False
bval (Less a1 a2)(memory) =
  if ((aval a1 memory) < (aval a2 memory)) then True
  else False

--TODO Task 2.1
data Com = Assign Vname AExp | Seq Com Com | If BExp Com Com | While BExp Com | SKIP
    deriving (Eq, Read, Show)

--TODO Task 2.4
eval :: Com -> State -> State
eval (Assign v x)(memory) = Data.Map.insert v (aval (x) (memory)) (memory)
eval (Seq c1 c2)(memory) = eval (c2) (eval c1 memory)
eval (If b c1 c2)(memory) =
  if ((bval b memory)==True) then eval c1 memory
  else eval c2 memory
eval (SKIP) (memory) = memory
eval (While b c1)(memory) =
  if ((bval b memory) == True) then eval (While b c1) (eval c1 memory)
  else memory
