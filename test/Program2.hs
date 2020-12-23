module Program2 where

import Lang
import IntProgram

program2 :: PROGRAM
program2 = Program
    { getPrDefinitions =
        [ Def "main" $ Fun "match" :@ Var "s" :@ Var "p"
        , Def "match" $ "s" :-> "p" :-> Fun "m1" :@ Var "p" :@ Var "s" :@ Var "p" :@ Var "s"
        , Def "m1" $ "x" :-> "s" :-> "op" :-> "os" :->
            Case (Var "x")
            [ Pat "Nil"  []          :=> Con "True" []
            , Pat "Cons" ["p", "pp"] :=> Fun "m2" :@ Var "s" :@ Var "p" :@ Var "pp" :@ Var "op" :@ Var "os"
            ]
        , Def "m2" $ "x" :-> "p" :-> "pp" :-> "op" :-> "os" :->
            Case (Var "x")
            [ Pat "Nil"  []          :=> Con "False" []
            , Pat "Cons" ["s", "ss"] :=> Case (ValP Eql [Var "s", Var "p"])
                                         [ Pat "True"  [] :=> Fun "m1" :@ Var "pp" :@ Var "ss" :@ Var "op" :@ Var "os"
                                         , Pat "False" [] :=> Fun "next" :@ Var "os" :@ Var "op"
                                         ]
            ]
        , Def "next" $ "x" :-> "p" :->
            Case (Var "x")
            [ Pat "Nil" [] :=> Var "x"
            , Pat "Cons" ["s", "ss"] :=> Fun "m1" :@ Var "p" :@ Var "ss" :@ Var "p" :@ Var "ss"
            ]
        ]
    , getPrEntryPoint = "main"
    }


prog2Tests :: TESTCASES
prog2Tests = [
        ( [ ("p", listToTerm [1,2,3]) ] :: COMPILEARGUMENTS
        , [ ("s", listToTerm [1,2,3]) ] :: EVALARGUMENTS
        , true                          :: EXPECTED
        )
        ,
        ( [ ("p", listToTerm [1,2,4]) ] 
        , [ ("s", listToTerm [1,2,3]) ] 
        , false                          
        )
        ,
        ( [ ("p", listToTerm [3,4]) ] 
        , [ ("s", listToTerm [0,1,2,3,4,6]) ] 
        , true                          
        )
        ,
        ( [ ("p", listToTerm [4, 0]) ] 
        , [ ("s", listToTerm [0,1,2,3,4,6]) ] 
        , false                          
        )
        ,
        ( [ ("s", listToTerm [1,2,3]) ]  
        , [ ("p", listToTerm [1,2,4]) ]
        , false                          
        )
        ,
        ( [ ("s", listToTerm [0,3,4]) ]  
        , [ ("p", listToTerm [3,4]) ]
        , true                          
        )
        ,
        ( [ ("s", listToTerm [0,1,2]) ]  
        , [ ("p", listToTerm [4, 0]) ]
        , false                          
        )
    ]
    where true = Con "True" []
          false = Con "False" []  