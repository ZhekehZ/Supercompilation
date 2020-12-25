module SubstTest where

import Lang
import Eval
import Instances
import IntProgram

subMain :: IO() 
subMain = do
    let got = subst (Var "x") "y" ("x" :-> Case (Var "x") [Pat "A" [] :=> Var "y"]) :: TERM
    let exp = "z" :-> Case (Var "z") [Pat "A" [] :=> Var "x"] :: TERM
    if got /= exp then
        putStrLn $ "Expected: " ++ show exp ++ "\nGot: " ++ show got ++ "\n-----"
    else 
        putStrLn "OK"


    let got = subst (Var "x") "y" (Case (Var "x") [Pat "A" [] :=> Var "y"]) :: TERM
    let exp = Case (Var "x") [Pat "A" [] :=> Var "x"] :: TERM
    if got /= exp then
        putStrLn $ "Expected: " ++ show exp ++ "\nGot: " ++ show got ++ "\n-----"
    else 
        putStrLn "OK"