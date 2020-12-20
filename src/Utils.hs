module Utils where

import Lang
import Data.List

type FuncCall val bf bp = (Either Name (Either bf bp), [Term val bf bp])

appAsFuncCall :: Term val bf bp -> Maybe (FuncCall val bf bp)
appAsFuncCall (a :@ b)      = (\(name, args) -> (name, args ++ [b])) <$> appAsFuncCall a
appAsFuncCall (Fun f)       = Just (Left f, [])
appAsFuncCall (ValF f args) = Just (Right (Left f), args)
appAsFuncCall (ValP p args) = Just (Right (Right p), args)
appAsFuncCall _             = Nothing

funcCallToApp :: FuncCall val bf bp -> Term val bf bp
funcCallToApp (Left f, args) = foldl (:@) (Fun f) args
funcCallToApp (Right (Left f), args) = ValF f args
funcCallToApp (Right (Right p), args) = ValP p args

-- Get list of free variables
getFree :: Term val bf bp -> [Name]
getFree term = case term of
    ValF _ args -> args >>= getFree
    ValP _ args -> args >>= getFree
    Con  _ args -> args >>= getFree
    x :-> t     -> filter (/= x) (getFree t)
    a :@ b      -> getFree a `union` getFree b
    Case e p    -> getFree e `union` (p >>= getFreePM)
    Let x e t   -> getFree e `union` filter (/= x) (getFree t)
    Var x       -> [x]
    _           -> []
    where
        getFreePM ((Pat _ args) :=> t) = getFree t \\ args


-- Rename variable `wh` to `to` in the term `term`
renameToIn :: Name -> Name -> Term val bf bp -> Term val bf bp
renameToIn wh to term = if wh == to then term else case term of
    (Val v)         -> Val v
    (Fun f)         -> Fun f
    (Var x)         -> Var $ if x == wh then to else x
    (ValF f args)   -> ValF f (whRenameToInTo <$> args)
    (ValP f args)   -> ValP f (whRenameToInTo <$> args)
    (Con  f args)   -> Con  f (whRenameToInTo <$> args)
    (x :-> t)       -> if x == wh then x :-> t else x :-> whRenameToInTo t
    (a :@ b)        -> whRenameToInTo a :@ whRenameToInTo b
    (Case e pmc)    -> Case (whRenameToInTo e) (whRenameToInToPM <$> pmc)
    (Let x e t)     -> Let x (whRenameToInTo e) (if x == wh then t else whRenameToInTo t)
    where
        whRenameToInTo = wh `renameToIn` to
        whRenameToInToPM (pm@(Pat _ args) :=> t) = pm :=> (if wh `elem` args then t else whRenameToInTo t)

showArgs as = case as of
    []       -> id
    (a : as) -> foldl (\s a -> s . showString ", " . shows a) (shows a) as


--------------------------------------------------
--  Instances
--------------------------------------------------

instance (Show val, Show bf, Show bp) => Show (Term val bf bp) where
    showsPrec p t = case t of
        Val v     -> shows v
        Var v     -> showString v
        Fun v     -> showString v
        ValF f as -> shows f . showParen True (showArgs as)
        ValP f as -> shows f . showParen True (showArgs as)
        Con  f as -> showString f . showParen True (showArgs as)
        a :@ b    -> showParen (p > 6) $ showsPrec 6 a . showChar ' ' . showsPrec 7 b
        a :-> b   -> showParen (p > 0) $ showString ('\\' : a ++ " -> ") . shows b
        Case e cs -> showString "case " . shows e . showString " of { " . showArgs cs . showString " }"
        Let x e e' -> showParen (p > 0) $ showString ("let " ++ x ++ " = ") . shows e . showString " in " . shows e'

instance (Show val, Show bf, Show bp) => Show (PatternMatchingCase val bf bp) where
    showsPrec _ (Pat c args :=> term) = showString c . showParen True (showArgs args) . showString " => " . shows term

instance (Eq val, Eq bf, Eq bp) => Eq (Term val bf bp) where
    Val x == Val x' = x == x'
    Var x == Var x' = x == x'
    Fun x == Fun x' = x == x'
    ValF f args == ValF f' args' = (f == f') && (args == args')
    ValP p args == ValP p' args' = (p == p') && (args == args')
    Con c args == Con c' args' = (c == c') && (args == args')
    (a :@ b) == (a' :@ b') = (a == a') && (b == b')
    (x :-> t) == (y :-> t') = renameToIn x y t == t'
    Case t cases == Case t' cases' = (t == t') && (cases == cases')
    Let x e t == Let y e' t' = (e == e') && ((x :-> t) == (y :-> t'))
    _ == _ = False

instance (Eq val, Eq bf, Eq bp) => Eq (PatternMatchingCase val bf bp) where
    (Pat c args :=> t) == (Pat c' args' :=> t') = (c == c')
            && (t == foldl (\t' (a', a) -> renameToIn a' a t') t' (zip args' args))


instance (Show val, Show bf, Show bp) => Show (Definition val bf bp) where
    showsPrec _ (Def name term) = showString (name ++ " = ") . shows term

instance (Show val, Show bf, Show bp) => Show (Program val bf bp) where
    showsPrec _ (Program defs entry) = showString ("Program (" ++ entry ++ "):")
            . foldl (\defs def -> defs . showString "\n\t" . shows def) id defs
