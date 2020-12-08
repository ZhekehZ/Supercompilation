module TestProgram where

import Lang

testProgram :: Program Int
testProgram = Program
    { getPrDefinitions =
        [ Def "sum" $ "xs" :-> "a" :->
            Case (Var "xs")
            [ Pat "Nil"  []          :=> Var "a"
            , Pat "Cons" ["x", "xs"] :=> Fun "sum" :@ Var "xs" :@ plus [Var "x", Var "a"]
            ]

        , Def "squares" $ "xs" :->
            Case (Var "xs")
            [ Pat "Nil"  []          :=> Con "Nil" []
            , Pat "Cons" ["x", "xs"] :=> Con "Cons" [mul [Var "x", Var "x"], Fun "squares" :@ Var "xs"]
            ]

        , Def "upto" $ "m" :-> "n" :->
            Case (gt [Var "m", Var "n"])
            [ Pat "True"   [] :=> Con "Nil" []
            , Pat "False"  [] :=> Con "Cons" [Var "m", Fun "upto" :@ plus [Var "m", Val 1] :@ Var "n"]
            ]

        , Def "main" $ Fun "sum" :@ (Fun "squares" :@ (Fun "upto" :@ Val 1 :@ Var "arg0")) :@ Val 0
        ]
    , getPrEntryPoint = "main"
    }
    where
        [plus, mul] = map (\f -> ValF $ \[x, y] -> f x y) [(+), (*)]
        [gt]        = map (\f -> ValP $ \[x, y] -> f x y) [(>)]
