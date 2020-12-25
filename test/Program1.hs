module Program1 where

import IntProgram
import Lang

program1 :: PROGRAM
program1 = Program
    { getPrDefinitions =
        [ Def "sum" $ "xs" :-> "a" :->
            Case (Var "xs")
            [ Pat "Nil"  []          :=> Var "a"
            , Pat "Cons" ["x", "xs"] :=> Fun "sum" :@ Var "xs" :@ ValF Plus [Var "x", Var "a"]
            ]

        , Def "squares" $ "xs" :->
            Case (Var "xs")
            [ Pat "Nil"  []          :=> Con "Nil" []
            , Pat "Cons" ["x", "xs"] :=> Con "Cons" [ValF Mul [Var "x", Var "x"], Fun "squares" :@ Var "xs"]
            ]

        , Def "upto" $ "m" :-> "n" :->
            Case (ValP Gt [Var "m", Var "n"])
            [ Pat "True"   [] :=> Con "Nil" []
            , Pat "False"  [] :=> Con "Cons" [Var "m", Fun "upto" :@ ValF Plus [Var "m", Val 1] :@ Var "n"]
            ]

        , Def "main" $ Fun "sum" :@ (Fun "squares" :@ (Fun "upto" :@ Val 1 :@ Var "arg0")) :@ Val 0
        ]
    , getPrEntryPoint = "main"
    }


prog1Tests :: TESTCASES
prog1Tests = [
        ( [ ("arg0", Val 1) ] :: COMPILEARGUMENTS
        , []                  :: EVALARGUMENTS
        , Val 1               :: EXPECTED
        )
        ,
        ( [ ("arg0", Val 2) ] 
        , []                  
        , Val 5               
        )
        ,
        ( [ ("arg0", Val 3) ] 
        , []                  
        , Val 14              
        )
        ,
        ( []                  
        , [ ("arg0", Val 1) ] 
        , Val 1               
        )
        ,
        ( []                  
        , [ ("arg0", Val 2) ] 
        , Val 5               
        )
        ,
        ( []                  
        , [ ("arg0", Val 3) ] 
        , Val 14              
        )
    ]
