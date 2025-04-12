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
import Machine ( State, Val, Vname )
import GHC.Unicode (GeneralCategory(OtherSymbol))

--TODO Task 2.1
data AExp =
    Plus AExp AExp| N Val | V Vname 
    deriving (Eq, Read, Show)

--TODO Task 2.2

aval :: AExp -> State-> Val
aval (N n) s =  n
aval (V v) s = s ! v
aval (Plus x y) s =
    j + k where
        j = aval x s
        k = aval y s

--TODO Task 2.1
data BExp=
    Bc Bool| Not BExp | And BExp BExp| Less AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp-> State -> Bool
bval (Bc a) s =
    a
bval (Less a b) s =
    aval a s < aval b s
bval (And a b) s =
    bval a s && bval b s
bval (Not a) s =
    not (bval a s)


--TODO Task 2.1
data Com =
    Assign Vname AExp | Seq Com Com | If BExp Com Com | While BExp Com | SKIP
    deriving (Eq, Read, Show)

--TODO Task 2.4

eval :: Com -> State -> State
eval (Assign v n ) s =
    --update state with v = n
     insert v (aval n s) s
eval (Seq c1 c2 ) s =
    eval c2 (eval c1 s)
eval (If b c1 c2) s
    | bval b s = eval c1 s 
    | otherwise = eval c2 s
eval (While b c) s--LOOP
    | bval b s = eval (While b c) (eval c s)
    | otherwise = s
eval SKIP s =
    s
    