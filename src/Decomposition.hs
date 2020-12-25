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
    (x:->y):@z   -> (Hole, term)  
    a :@ b       -> let (ctx, e') = decompose a in (ctx :@: b, e')
    Case ce xs   -> let (ctx, e') = decompose ce in (CCase ctx xs, e') 
    _            -> (Hole, term)  

isObservable :: Term val bf pb -> Bool
isObservable term = case term of
    Val   _ -> True
    Con _ _ -> True
    _ :-> _ -> True
    _       -> False
