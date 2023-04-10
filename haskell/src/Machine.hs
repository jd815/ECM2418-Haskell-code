module Machine
(
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec,
) where

import Data.Map

type Vname = String
type Val = Int
type State = Data.Map.Map Vname Val

data Instr = LOADI Int | LOAD String | ADD | STORE Vname| JMP Int| JMPLESS Int| JMPGE Int
        deriving (Eq, Read, Show)

type Stack = [Val]

type Config = (Int, State, Stack)

addDel :: [Val] -> [Val]
addDel (x:y:xs) = x + y  : xs




iexec :: Instr -> Config -> Config
iexec (LOADI x)(counter, memory, stack) =  (counter+1, memory, x:stack)
iexec (LOAD x)(counter, memory, stack) = (counter +1, memory, stack ++ [memory ! x])
iexec (ADD)(counter, memory, stack) = (counter +1, memory, addDel stack)
iexec (STORE x)(counter, memory, stack) = (counter +1, Data.Map.insert x (head stack) (memory), init stack)
iexec (JMP x)(counter, memory, stack) = (counter+x+1, memory, stack)
iexec (JMPLESS x)(counter, memory, stack) =
  if head (stack) > (stack !! 1) then (counter+x+1, memory, init (init stack))
  else (counter+1, memory, init (init stack))
iexec (JMPGE x)(counter, memory, stack) =
  if head (stack) < (stack !! 1) then (counter+x+1, memory, init (init stack))
  else if head (stack) == (stack !! 1) then (counter+x+1, memory, init (init stack))
  else (counter+1, memory, init (init stack))

exec :: [Instr] -> Config -> Config
exec [x](counter, memory, stack) = iexec x (counter, memory, stack)
exec (x:xs)(counter, memory, stack) = exec xs (iexec x (counter, memory, stack))
