module TestProgram where

import Lang
import Utils

data BuiltinF = Plus | Mul
    deriving (Show, Eq)
data BuiltinP = Gt
    deriving (Show, Eq)

type BFE = BuiltinFunctionEval Int BuiltinF
type BPE = BuiltinPredicateEval Int BuiltinP
type PROGRAM = Program Int BuiltinF BuiltinP
type TERM = Term Int BuiltinF BuiltinP

evalIntF :: BFE
evalIntF bf args = case (bf, args) of
  (Plus, [x, y]) -> x + y
  (Mul,  [x, y]) -> x * y
  _  -> error ("Invalid function call: " ++ show bf ++ ", arguments = " ++ show args ++ ")")

evalIntP :: BPE
evalIntP bp args = case (bp, args) of
  (Gt,   [x, y]) -> x > y
  _  -> error ("Invalid predicate call: " ++ show bp ++ ", arguments = " ++ show args ++ ")")

testProgram :: PROGRAM
testProgram = Program
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
