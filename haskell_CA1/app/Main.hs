module Main where

import System.Environment
import Compiler
import Machine
import Interpreter

--TODO Task 3.4

--"Assign \"x\" (Plus (N 5)(N 3)) "

--Assign Vname AExp | Seq Com Com | If BExp Com Com | While BExp Com | SKIP

main :: IO ()
main = do
    line <- getLine
    let x = (read line :: Com)
    print (ccomp x)
