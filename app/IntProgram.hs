module IntProgram where

import Lang
import Utils
import Eval
import Driving
import ProcessTree
import CodeGeneration
import Generalization
import Data.Maybe

data BuiltinF = Plus | Minus | Mul | Div | Mod  deriving (Show, Eq)
data BuiltinP = Gt | Eql | Lt                   deriving (Show, Eq)

type BFE = BuiltinFunctionEval Int BuiltinF
type BPE = BuiltinPredicateEval Int BuiltinP
type PROGRAM = Program Int BuiltinF BuiltinP
type TERM = Term Int BuiltinF BuiltinP
type DEFINITION = Definition Int BuiltinF BuiltinP

evalIntF :: BFE
evalIntF bf args = case (bf, args) of
  (Plus , [x, y]) -> x + y
  (Minus, [x, y]) -> x - y
  (Mul  , [x, y]) -> x * y
  (Div  , [x, y]) -> x `div` y
  (Mod  , [x, y]) -> x `mod` y
  _  -> error ("Invalid function call: " ++ show bf ++ ", arguments = " ++ show args ++ ")")

evalIntP :: BPE
evalIntP bp args = case (bp, args) of
  (Gt,   [x, y]) -> x > y
  (Eql,  [x, y]) -> x == y
  (Lt,   [x, y]) -> x < y
  _  -> error ("Invalid predicate call: " ++ show bp ++ ", arguments = " ++ show args ++ ")")


context = EC evalIntF evalIntP

listToTerm :: [String] -> TERM
listToTerm []       = Con "Nil" []
listToTerm (x : xs) = Con "Cons" [Con x [], listToTerm xs]


strToTerm :: String -> TERM
strToTerm = listToTerm . map (:[])

termToStr :: TERM -> Maybe String
termToStr (Con "Nil"  []) = Just ""
termToStr (Con "Cons" [Con [l] [], rest]) | Just rs <- termToStr rest = Just $ l : rs
termToStr _ = Nothing

repr :: TERM -> String
repr t = fromMaybe (show t) (termToStr t) 

applyDefaults :: PROGRAM -> [(Name, TERM)] -> PROGRAM
applyDefaults (Program defs entry) defaults = 
    let doSubs = applySubstitution $ uncurry (:=) <$> defaults 
        newDefs = (\(Def name term) -> Def name $ doSubs term) <$> defs
    in Program newDefs entry

evalEntry :: PROGRAM -> PROGRAM
evalEntry prog@(Program defs entry) = let others = filter (\(Def name _) -> name /= entry) defs
                                      in Program (Def entry (eval prog []) : others) entry

-- SuperCompilation
compile :: PROGRAM -> [(Name, TERM)] -> PROGRAM
compile prog defaults =  compileTree (buildProgramTree context $ applyDefaults prog defaults)

-- Evaluation
eval :: PROGRAM -> [(Name, TERM)] -> TERM
eval prog defaults = evalProgram (applyDefaults prog defaults) context

tree :: PROGRAM -> [(Name, TERM)] -> Tree (Node Int BuiltinF BuiltinP)
tree prog defaults = buildProgramTree (EC evalIntF evalIntP) (applyDefaults prog defaults)

treeN :: PROGRAM -> [(Name, TERM)] -> Int -> Tree (Node Int BuiltinF BuiltinP)
treeN prog defaults = buildProgramTreeN (EC evalIntF evalIntP) (applyDefaults prog defaults)

type COMPILEARGUMENTS = [(Name, TERM)]
type EVALARGUMENTS = [(Name, TERM)]
type EXPECTED = TERM

type TESTCASES = [ (COMPILEARGUMENTS, EVALARGUMENTS, EXPECTED) ]
type TESTMATRIX = ([COMPILEARGUMENTS], [EVALARGUMENTS], Bool)
