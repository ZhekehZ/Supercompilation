module Utils where

import Lang
import Decomposition
import Data.List
import ProcessTree

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
getFree term = nub $ case term of
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


lookupFun :: [Definition val bf bp] -> Name -> Term val bf bp
lookupFun (Def defName expr : def) name | name == defName = expr
                                        | otherwise       = lookupFun def name
lookupFun [] name = error $ "Invalid function name: " ++ name

showArgs as = case as of
    []       -> id
    (a : as) -> foldl (\s a -> s . showString ", " . shows a) (shows a) as

isSet :: Eq a => [a] -> Bool
isSet xs = null $ xs \\ nub xs
