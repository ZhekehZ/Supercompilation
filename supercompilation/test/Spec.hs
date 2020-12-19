import TestProgram
import Eval
import Lang

term1 :: TERM
term1 = Fun "f1" :@ (Fun "f2" :@ Var "x") :@ Var "x"

term2 :: TERM
term2 = Fun "f1" :@ (Fun "f3" :@ (Fun "f2" :@ Var "x") :@ Var "y") :@ Val 0

main :: IO ()
--main = print $ evalProgram testProgram (EC [("arg0", 2)] evalIntF evalIntP)
main = print $ term1 ## term2


{-
(
    Fun "f1" :@ Var "v" :@ Var "v'",
[("v",Fun "f2" :@ Var "x"), ("v", Var "x")],
[("v", (Fun "f3" :@ (Fun "f2" :@ Var "x")) :@ Var "y"), ("v",Val 0)])

-}