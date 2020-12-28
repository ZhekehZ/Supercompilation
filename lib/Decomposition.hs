module Decomposition where

import Lang
import Utils
import Eval

data Context val bf bp = Hole
                       | Context val bf bp :@: Term val bf bp
                       | CCase (Context val bf bp) (PatternMatching val bf bp)

infixl 1 :@:

type Decomposition val bf bp = (Context val bf bp, Term val bf bp, Maybe [(Name, [Name])])

fillHole :: Context val bf bp -> Term val bf bp -> Term val bf bp
fillHole Hole t = t
fillHole (c :@: e) t = fillHole c t :@ e
fillHole (CCase c pm) t = Case (fillHole c t) pm


fillCaseHole :: Context val bf bp -> Name -> Term val bf bp
fillCaseHole (c :@: e) x = fillCaseHole c x :@ e
fillCaseHole (CCase Hole pm) c | Just (Pat _ ns :=> t) <- findPMCase c pm = t
fillCaseHole (CCase c pm) x = Case (fillCaseHole c x) pm

decompose :: Term val bf bp -> Decomposition val bf bp
decompose term = case term of
    a :@ b -> let (ctx, e', pm) = decompose a in (ctx :@: b, e', pm)
    Case e pm | (Hole, e', Nothing) <- decompose e -> (CCase Hole pm, e', Just $ (\(Pat c ns :=> _) -> (c, ns)) <$> pm)  
              | (ctx,  e',     mpm) <- decompose e -> (CCase ctx  pm, e', mpm)
    _ -> (Hole, term, Nothing)    