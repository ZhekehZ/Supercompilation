module Decomposition where

import Lang

data Context val bf bp = Hole
                       | Context val bf bp :@: Term val bf bp
                       | CCase (Context val bf bp) (PatternMatching val bf bp)

infixl 1 :@:

type Decomposition val bf bp = (Context val bf bp, Term val bf bp)

fillHole :: Context val bf bp -> Term val bf bp -> Term val bf bp
fillHole Hole t = t
fillHole (c :@: e) t = fillHole c t :@ e
fillHole (CCase c pm) t = Case (fillHole c t) pm

decompose :: Term val bf bp -> Decomposition val bf bp
decompose term = case term of
    (ValF _ _) -> (Hole, term)
    (ValP _ _) -> (Hole, term)
    (Var    _) -> (Hole, term)
    (e1 :@ e2) -> case decompose e1 of (ctx, term) -> (ctx :@: e2, term)
    (Case e p) -> case decompose e of (ctx, term) -> (CCase ctx p, term)
    _          -> error "The term is in NF or evaluatable"

isObservable :: Term val bf pb -> Bool
isObservable term = case term of
    (Val   _) -> True
    (Con _ _) -> True
    (_ :-> _) -> True
    _         -> False

getCaseVariants :: Context val bf bp -> [(Name, Int)]
getCaseVariants Hole            = []
getCaseVariants (a :@: b)       = getCaseVariants a
getCaseVariants (CCase Hole pm) = [(c, length args) | Pat c args :=> t <- pm]
getCaseVariants (CCase  ctx pm) = getCaseVariants ctx