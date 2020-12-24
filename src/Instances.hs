module Instances where

import Utils
import Eval
import Lang
import ProcessTree
import Decomposition

newtype RawString = RawString String

instance Show RawString where
    showsPrec p (RawString s) = showString s

instance (Show val, Show bf, Show bp) => Show (Term val bf bp) where
    showsPrec p t = case t of
        Val v     -> shows v
        Var v     -> showString v
        Fun v     -> showString v
        ValF f as -> shows f . showParen True (showArgs as)
        ValP f as -> shows f . showParen True (showArgs as)
        Con  f as -> showString f . if null as then id else showParen True (showArgs as)
        a :@ b    -> showParen (p > 6) $ showsPrec 6 a . showChar ' ' . showsPrec 7 b
        a :-> b   -> showParen (p > 0) $ showString ('\\' : a ++ " -> ") . shows b
        Case e cs -> showString "case " . shows e . showString " of { " . showArgs cs . showString " }"
        Let x e e' -> showParen (p > 0) $ showString ("let " ++ x ++ " = ") . shows e . showString " in " . shows e'

instance (Show val, Show bf, Show bp) => Show (PatternMatchingCase val bf bp) where
    showsPrec _ (Pat c args :=> term) = showString c . (if null args then id
                                else showParen True (showArgs (RawString <$> args))) . showString " => " . shows term




instance (Show val, Show bf, Show bp) => Show (Definition val bf bp) where
    showsPrec _ (Def name term) = showString (name ++ " = ") . shows term

instance (Show val, Show bf, Show bp) => Show (Program val bf bp) where
    showsPrec _ (Program defs entry) = showString ("Program (" ++ entry ++ ", args = ")
            . showArgs ( concatMap (getFree . (\(Def _ t) -> t)) defs) . showString "):"
            . foldl (\defs def -> defs . showString "\n\t" . shows def) id defs

instance (Show val, Show bf, Show bp) => Show (Context val bf bp) where
    showsPrec _ c = case c of
      Hole       -> showString "<.>"
      c :@: t    -> shows c . showChar ' ' . showsPrec 7 t
      CCase c cs -> showString "case " . shows c . showString " of { " . showArgs cs . showString " }"

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
