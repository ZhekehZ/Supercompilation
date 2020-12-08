module Eval where

import Lang
import Data.List

import Control.Applicative

type Context val = [(Name, val)]

evalProgram :: Program val -> Context val -> Term val
evalProgram (Program def entry) args = evalExpr args def (lookupFun def entry)

lookupFun :: [Definition val] -> Name -> Term val
lookupFun (Def defName expr : def) name | name == defName = expr
                                        | otherwise       = lookupFun def name
lookupFun [] name = error $ "Invalid function name: " ++ name

getFree :: Term val -> [Name]
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


renameToIn :: Name -> Name -> Term val -> Term val
renameToIn wh to term = case term of
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


subst :: Term val -> Name -> Term val -> Term val
subst new name expr = case expr of
    Val v       -> Val v
    Fun f       -> Fun f
    Var x       -> if x == name then new else Var x
    ValF f args -> ValF f (substThis <$> args)
    ValP p args -> ValP p (substThis <$> args)
    Con  c args -> Con  c (substThis <$> args)
    x :-> t     -> let x' = assertFree x (getFree t \\ [x]) in x' :-> substThis (renameToIn x x' t)
    Let x e t   -> let x' = assertFree x (getFree t \\ [x]) in Let x' (substThis e) (substThis (renameToIn x x' t))
    e1 :@ e2    -> substThis e1 :@ substThis e2
    Case e pmc  -> Case (substThis e) (substThisPM <$> pmc)
    where
        freeInNew = getFree new
        assertFree x free = if x `elem` freeInNew || x `elem` free then assertFree (x ++ "'") free else x
        substThis = subst new name

        substThisPM (Pat c args :=> t) =
          let uniArgs = (\(_, n) -> "arg" ++ show n) <$> zip args [1 :: Int .. ]
              freArgs = (\x -> assertFree x (getFree t)) <$> uniArgs
          in Pat c freArgs :=> substThis (foldl (\term (wh, to) -> renameToIn wh to term) t (zip args freArgs))



lookupPM :: Name -> PatternMatching val -> Maybe (PatternMatchingCase val)
lookupPM c (pmc@(Pat c' _ :=> _) : pms) | c == c'   = Just pmc
                                        | otherwise = lookupPM c pms
lookupPM _ _ = Nothing


evalExprStep :: Context val -> [Definition val] -> Term val -> Maybe (Term val)
evalExprStep context defines expression = case expression of
  ValF f args  -> (ValF f <$> evalArguments args) <|> Just (Val $ f (getValue <$> args))
  ValP p args  -> (ValP p <$> evalArguments args) <|> Just (Con (show $ p (getValue <$> args)) [])
  Var v        -> Val <$> lookup v context
  Fun f        -> Just $ lookupFun defines f
  ((x:->e):@t) -> Just $ subst t x e
  (e1:@e2)     -> (:@e2) <$> eval e1
  (Let x e t)  -> (flip (Let x) t <$> eval e) <|> Just (subst e x t)
  (Case e pms) -> (flip Case pms <$> eval e) <|>
                    case e of
                      Con c args -> case lookupPM c pms of
                          Just (Pat _ argNames :=> t) ->
                              Just $ foldl (\term (name, value) -> subst value name term) t (zip argNames args)
                          Nothing -> error "Invalid pattern-matching"
                      _ -> Nothing
  _ -> Nothing

  where
      eval = evalExprStep context defines
      evalArguments (x:xs) = ((:xs) <$> eval x) <|> ((x:) <$> evalArguments xs)
      evalArguments []     = Nothing


evalExpr :: Context val -> [Definition val] -> Term val -> Term val
evalExpr context def t = maybe t (evalExpr context def) (evalExprStep context def t)

