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
acomp (N a) = [LOADI a]
acomp (V v) = [LOAD v]
acomp (Plus a b) = acomp a ++ acomp b ++ [ADD]

-- !Completely bodged
--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Less a b) bo i
    | bo  = acomp a ++ acomp b ++ [JMPLESS i] 
    | otherwise = acomp a ++ acomp b ++ [JMPGE i]
bcomp (And (Bc a) (Bc b)) bo i
    | (a && b) == bo = [JMP i]
    | otherwise   = []

-- ! may contain bugs
bcomp (And (Bc a) ((Less b c))) bo i
    | not a == bo = [JMP i] ++ bcomp (Less b c) bo i
    | otherwise = [JMP (i*2)] ++ bcomp (Less b c) bo i

bcomp (And a b) bo i =
    bcomp a bo i ++ bcomp b bo i
bcomp (Bc b) bo i
    | b == bo = [JMP i]
    | otherwise = []
bcomp b bo i =
    []
    
--TODO Task 3.3
--Assign Vname AExp | Seq Com Com | If BExp Com Com | While BExp Com | SKIP
ccomp :: Com -> [Instr]
ccomp (Assign a b) =
     acomp b ++ [STORE a]
ccomp (Seq c1 c2) =
    ccomp c1 ++ ccomp c2
ccomp (If b c1 c2) =
    bcomp b False y  ++ ccomp c1 ++ [JMP x] ++ ccomp c2
    where
        x = length (ccomp c2)
        y = length (ccomp c1 ++ [JMP x])
ccomp (While b c) =
    bcomp b False y  ++ ccomp c ++ [JMP (-x)]
    where
        x = length (bcomp b False 0  ++ ccomp c) +1
        y = length (ccomp c ++ [JMP (-x)])
ccomp SKIP =
    [JMP 1]


