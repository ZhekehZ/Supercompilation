module Program2 where

{-
    KMP test
-}

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
            , Pat "Cons" ["s", "ss"] :=> Case (Fun "cmp" :@ Var "s" :@ Var "p")
                                         [ Pat "True"  [] :=> Fun "m1" :@ Var "pp" :@ Var "ss" :@ Var "op" :@ Var "os"
                                         , Pat "False" [] :=> Fun "next" :@ Var "os" :@ Var "op"
                                         ]
            ]
        , Def "next" $ "x" :-> "p" :->
            Case (Var "x")
            [ Pat "Cons" ["s", "ss"] :=> Fun "m1" :@ Var "p" :@ Var "ss" :@ Var "p" :@ Var "ss"
            ]
        , Def "cmp" $ "x" :-> "y" :-> 
            Case (Var "x")
            [ Pat "A" [] :=> Case (Var "y") [ Pat "A" [] :=> Con "True" [], Pat "B" [] :=> Con "False" [], Pat "C" [] :=> Con "False" []]
            , Pat "B" [] :=> Case (Var "y") [ Pat "A" [] :=> Con "False" [], Pat "B" [] :=> Con "True" [], Pat "C" [] :=> Con "False" []]
            , Pat "C" [] :=> Case (Var "y") [ Pat "A" [] :=> Con "False" [], Pat "B" [] :=> Con "False" [], Pat "C" [] :=> Con "True" []]
            ]
        ]
    , getPrEntryPoint = "main"
    }


prog2Tests :: TESTCASES
prog2Tests = [
        ( [ ("p", strToTerm "ABC") ] :: COMPILEARGUMENTS
        , [ ("s", strToTerm "ABC") ] :: EVALARGUMENTS
        , true                       :: EXPECTED
        )
        ,
        ( [ ("p", strToTerm "ABB") ] 
        , [ ("s", strToTerm "ABC") ] 
        , false                          
        )
        ,
        ( [ ("p", strToTerm "AB") ] 
        , [ ("s", strToTerm "CCCABCCC") ] 
        , true                          
        )
        ,
        ( [ ("p", strToTerm "CA") ] 
        , [ ("s", strToTerm "ABBAAB") ] 
        , false                          
        )
        ,
        ( [ ("s", strToTerm "ABC") ] 
        , [ ("p", strToTerm "ABB") ]
        , false                          
        )
    ]
    where true = Con "True" []
          false = Con "False" []  


prog2MatrixTests :: TESTMATRIX
prog2MatrixTests = (
    (:[]) . (,) "p" . strToTerm <$> 
        [ "A"
        , "AA"
        , "AAA"
        , "AB"
        , "ABB"
        , "AAB"
        , "ABC"
        , "ABAC"
        , "AAAB"
        , "ABABAB"
        , "ABCAABB"
        , "AAABAAA"
        , "AAABACA"
        , "AAABACBB"
        ] :: [COMPILEARGUMENTS]
    , 
    (:[]) . (,) "s" . strToTerm <$> 
        [ "BAC"
        , "AAAAAAA"
        , "AAAABBBBBBCCCCC"
        , "BABABABABACBABABABAB"
        , "CCCCCCAAAAAAAACCCCCCAAA"
        , "BABABABABABCAABBBABABAB"
        , "CCCCCAAAAABAAAABACACCC"
        , "BACBBACAAABACBBAC"
        ] :: [EVALARGUMENTS]
    ,
    True
    )

prog2MatrixTestsRev :: TESTMATRIX
prog2MatrixTestsRev = (
    (:[]) . (,) "s" . strToTerm <$> 
        [ "A"
        , "AA"
        , "AAA"
        , "AB"
        , "ABB"
        , "AAB"
        , "ABC"
        , "ABAC"
        , "AAAB"
        , "AABB"
        , "BABACC"
        ] :: [COMPILEARGUMENTS]
    , 
    (:[]) . (,) "p" . strToTerm <$> 
        [ "A"
        , "AA"
        , "AB"
        , "AC"
        , "BA"
        ] :: [EVALARGUMENTS]
    ,
    True
    )