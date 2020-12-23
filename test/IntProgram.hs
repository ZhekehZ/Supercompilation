module IntProgram where

import Lang
import Utils
import Eval
import Driving
import CodeGeneration
import Generalization

data BuiltinF = Plus | Minus | Mul   deriving (Show, Eq)
data BuiltinP = Gt | Eql             deriving (Show, Eq)

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


context = EC evalIntF evalIntP

listToTerm :: [Int] -> TERM
listToTerm []       = Con "Nil" []
listToTerm (x : xs) = Con "Cons" [Val x, listToTerm xs]

applyDefaults :: PROGRAM -> [(Name, TERM)] -> PROGRAM
applyDefaults (Program defs entry) defaults = 
    let doSubs = applySubstitution $ uncurry (:=) <$> defaults 
        newDefs = (\(Def name term) -> Def name $ doSubs term) <$> defs
    in Program newDefs entry

-- evalEntry :: PROGRAM -> PROGRAM
-- evalEntry prog@(Program defs entry) = let others = filter (\(Def name _) -> name /= entry) defs
--                                       in Program (Def entry (eval prog []) : others) entry

-- SuperCompilation
compile :: PROGRAM -> [(Name, TERM)] -> PROGRAM
compile prog defaults =  compileTree (buildProgramTree context $ applyDefaults prog defaults)

-- Evaluation
eval :: PROGRAM -> [(Name, TERM)] -> TERM
eval prog defaults = evalProgram (applyDefaults prog defaults) context


type COMPILEARGUMENTS = [(Name, TERM)]
type EVALARGUMENTS = [(Name, TERM)]
type EXPECTED = TERM
type TESTCASES = [ (COMPILEARGUMENTS, EVALARGUMENTS, EXPECTED) ]