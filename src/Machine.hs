module Machine
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map

--TODO Task 1.1
type Vname = String
--TODO Task 1.2
type Val = Int
--TODO Task 1.3
type State = Map Vname Val

--TODO Task 1.4
data Instr =
    LOADI Val | LOAD Vname | ADD | STORE Vname | JMP Int | JMPLESS Int | JMPGE Int
        deriving (Eq, Read, Show)

--TODO Task 1.5
type Stack = [Val]
--TODO Task 1.6

--1 = PC, 2 = State , 3 = Stack
type Config = (Int, State, Stack)

--TODO Task 1.7----------------------------------------------------

add :: Num a => [a] -> a
add xs
  = sum(Prelude.take 2 xs)

-- ! POLISH!!!!!
iexec :: Instr -> Config -> Config
iexec (LOADI x) (pc,state,stack)
  = (pc+1,state,x:stack)

iexec (LOAD s) (pc,state,stack)
  = (pc+1,state, j:stack )
  where j = state ! s
  
iexec ADD (pc,state,stack)
  = (pc+1, state, add stack:tail (tail stack) )
  
iexec (STORE s) (pc,state,stack)
  = (pc+1, insert s (head stack) state, tail stack )
  
iexec (JMP i) (pc,state,stack)
  = (pc+i+1, state, stack)
  
iexec (JMPLESS i) (pc,state,stack)
  | head stack < head (tail stack) = (pc+i+1, state, [])
  | otherwise = (pc+i+1, state, [])
  
iexec (JMPGE i) (pc,state,stack)
  | head stack >= head (tail stack) = (pc+i+1, state, [])
  | otherwise = (pc+i+1, state, [])

--TODO Task 1.8 
exec :: [Instr] -> Config -> Config
exec [] c
  = c
exec (i:is) c
  = exec is (iexec i c)