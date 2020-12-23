module TestProgram where

import Lang
import Utils
import Eval
import Driving
import CodeGeneration

data BuiltinF = Plus | Minus | Mul
    deriving (Show, Eq)
data BuiltinP = Gt | Eql
    deriving (Show, Eq)

type BFE = BuiltinFunctionEval Int BuiltinF
type BPE = BuiltinPredicateEval Int BuiltinP
type PROGRAM = Program Int BuiltinF BuiltinP
type TERM = Term Int BuiltinF BuiltinP

evalIntF :: BFE
evalIntF bf args = case (bf, args) of
  (Plus , [x, y]) -> x + y
  (Minus, [x, y]) -> x - y
  (Mul  , [x, y]) -> x * y
  _  -> error ("Invalid function call: " ++ show bf ++ ", arguments = " ++ show args ++ ")")

evalIntP :: BPE
evalIntP bp args = case (bp, args) of
  (Gt,   [x, y]) -> x > y
  (Eql,  [x, y]) -> x == y
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


emptyContext = EC [] evalIntF evalIntP
EC d f p <# x = EC (x:d) f p

testProgram2 :: PROGRAM
testProgram2 = Program
    { getPrDefinitions =
        [ Def "main" $ Fun "match" :@ Var "s" :@ Var "p" -- (Con "Cons" [Val 1, Con "Nil" []])
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
            [ Pat "Cons" ["s", "ss"] :=> Fun "m1" :@ Var "p" :@ Var "ss" :@ Var "p" :@ Var "ss"
            ]
        ]
    , getPrEntryPoint = "main"
    }


listToTerm :: [Int] -> TERM
listToTerm []       = Con "Nil" []
listToTerm (x : xs) = Con "Cons" [Val x, listToTerm xs]

fun1 :: [Int] -> [Int] -> Bool
fun1 = \p -> \s -> case p of
            [] -> True
            arg1':arg2' -> case s of
                [] -> False
                arg1:arg2 -> case arg1 == arg1' of
                    True -> fun1 arg2' arg2
                    False -> case s of
                            arg1:arg2' -> fun1 p arg2'


compile args prog = compileTree (buildProgramTree (foldl (<#) emptyContext args) prog)