module TermEq where

import Utils
import Eval
import Lang

instance (Eq val, Eq bf, Eq bp) => Eq (Term val bf bp) where
    Val x == Val x' = x == x'
    Var x == Var x' = x == x'
    Fun x == Fun x' = x == x'
    ValF f args == ValF f' args' = (f == f') && (args == args')
    ValP p args == ValP p' args' = (p == p') && (args == args')
    Con c args == Con c' args' = (c == c') && (args == args')
    (a :@ b) == (a' :@ b') = (a == a') && (b == b')
    (x :-> t) == (y :-> t') = subst (Var y) x t == t'
    Case t cases == Case t' cases' = (t == t') && (cases == cases')
    Let x e t == Let y e' t' = (e == e') && ((x :-> t) == (y :-> t'))
    _ == _ = False


instance (Eq val, Eq bf, Eq bp) => Eq (PatternMatchingCase val bf bp) where
    (Pat c args :=> t) == (Pat c' args' :=> t') = (c == c')
            && (t == foldl (\t' (a', a) -> subst (Var a) a' t') t' (zip args' args))
