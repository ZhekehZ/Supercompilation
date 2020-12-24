module Eval where

import Lang
import Utils
import Data.List
import Control.Applicative
import Decomposition

data EvalContext val bf bp = EC (BuiltinFunctionEval val bf) (BuiltinPredicateEval val bp)

-- evaluate program in given evaluation context
evalProgram :: Program val bf bp -> EvalContext val bf bp -> Term val bf bp
evalProgram (Program def entry) context = evalExpr context def (lookupFun def entry)

-- Substitutuion
subst :: Term val bf bp -> Name -> Term val bf bp -> Term val bf bp
subst new name expr = case expr of
    Val v       -> Val v
    Fun f       -> Fun f
    Var x       -> if x == name then new else Var x
    ValF f args -> ValF f (substThis <$> args)
    ValP p args -> ValP p (substThis <$> args)
    Con  c args -> Con  c (substThis <$> args)
    x :-> t     -> if x == name then expr else
                   if x `elem` freeInNew then
                        let x' = assertFree x (getFree t \\ [x]) in x' :-> substThis (subst (Var x') x t)
                   else x :-> substThis t
    Let x e t   -> case substThis ((x :-> t) :@ e) of ((x :-> t) :@ e) -> Let x e t
    e1 :@ e2    -> substThis e1 :@ substThis e2
    Case e pmc  -> Case (substThis e) (substThisPM <$> pmc)
    where
        freeInNew = getFree new
        substThis = subst new name
        assertFree x free = if x `elem` freeInNew || x `elem` free then assertFree (x ++ "'") free else x

        substThisPM pat@(Pat c args :=> t) = if any (== name) args then pat else
            let uniArgs = (\(_, n) -> "a" ++ show n) <$> zip args [1 :: Int .. ]
                freArgs = (\x -> assertFree x (getFree t)) <$> uniArgs
            in Pat c freArgs :=> substThis (foldl (\term (wh, to) -> subst (Var to) wh term) t (zip args freArgs))


-- Find pattern-matching case by contructor name
lookupPM :: Name -> PatternMatching val bf bp -> Maybe (PatternMatchingCase val bf bp)
lookupPM c (pmc@(Pat c' _ :=> _) : pms) | c == c'   = Just pmc
                                        | otherwise = lookupPM c pms
lookupPM _ _ = Nothing

-- Evaluate expression with given context and function definitions
evalExpr :: EvalContext val bf bp -> [Definition val bf bp] -> Term val bf bp -> Term val bf bp
evalExpr context def t = maybe t (evalExpr context def) (evalExpr1 context def t)

-- Evaluation step
evalExpr1 :: EvalContext val bf bp -> [Definition val bf bp] -> Term val bf bp -> Maybe (Term val bf bp)
evalExpr1 evalContext@(EC bfEval bpEval) defines expression = case expression of
  ValF f args  -> (ValF f <$> evalArguments args) <|> ((Val . bfEval f) <$> traverse tryGetValue args)
  ValP p args  -> (ValP p <$> evalArguments args) <|>
                                            ((flip Con [] . show . bpEval p) <$> traverse tryGetValue args)
  Var v        -> Nothing
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
  _            -> Nothing
  where
      eval = evalExpr1 evalContext defines
      evalArguments (x:xs) = ((:xs) <$> eval x) <|> ((x:) <$> evalArguments xs)
      evalArguments []     = Nothing
      tryGetValue (Val v) = Just v
      tryGetValue _ = Nothing
