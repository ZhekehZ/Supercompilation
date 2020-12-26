module Eval where

import Lang
import Utils
import Data.List
import Control.Applicative

data EvalContext val bf bp = EC (BuiltinFunctionEval val bf) (BuiltinPredicateEval val bp)

-- evaluate program in given evaluation context
evalProgram :: Program val bf bp -> EvalContext val bf bp -> Term val bf bp
evalProgram (Program def entry) context = evalExpr context def (lookupFun def entry)

rename :: Name -> Name -> Term val bf bp -> Term val bf bp
rename x y term | x == y = term 
                | otherwise = subst (Var y) x term

-- Substitutuion
subst :: Term val bf bp -> Name -> Term val bf bp -> Term val bf bp
subst new name expr = case expr of
    Val v       -> Val v
    Fun f       -> Fun f
    Var x       -> if x == name then new else Var x
    ValF f args -> ValF f (substThis <$> args)
    ValP p args -> ValP p (substThis <$> args)
    Con  c args -> Con  c (substThis <$> args)
    x :-> t     -> if x == name then expr else let [x'] = getFreeName (getFree expr ++ getFree new) [x] 
                                               in x' :-> substThis (rename x x' t)
    Let x e t   -> case substThis ((x :-> t) :@ e) of ((x :-> t) :@ e) -> Let x e t
    e1 :@ e2    -> substThis e1 :@ substThis e2
    Case e pmc  -> Case (substThis e) (substThisPM <$> pmc)
    where
        substThis = subst new name
        substThisPM pat@(Pat c args :=> t) = if name `elem` args then pat else 
            let (a', t') = foldr (\n (ars, t) -> let [n'] = getFreeName (name : getFree new ++ getFree t ++ ars) [n] in (n':ars, rename n n' t)) ([], t) args
            in Pat c a' :=> substThis t' 

-- Evaluate expression with given context and function definitions
evalExpr :: EvalContext val bf bp -> [Definition val bf bp] -> Term val bf bp -> Term val bf bp
evalExpr context def t = maybe t (evalExpr context def) (evalExpr1 context def t)

evalExprN :: EvalContext val bf bp -> [Definition val bf bp] -> Term val bf bp -> Int -> Term val bf bp
evalExprN context def t 0 = t
evalExprN context def t n = maybe t (flip (evalExprN context def) (n - 1)) (evalExpr1 context def t)


-- Evaluation step
evalExpr1 :: EvalContext val bf bp -> [Definition val bf bp] -> Term val bf bp -> Maybe (Term val bf bp)
evalExpr1 evalContext@(EC bfEval bpEval) defines expression = case expression of
  ValF f args  -> (ValF f <$> evalArguments args) <|> (Val . bfEval f <$> traverse tryGetValue args)
  ValP p args  -> (ValP p <$> evalArguments args) <|>
                                            (flip Con [] . show . bpEval p <$> traverse tryGetValue args)
  Var v        -> Nothing
  Fun f        -> Just $ lookupFun defines f
  ((x:->e):@t) -> Just $ subst t x e
  (e1:@e2)     -> (:@e2) <$> eval e1
  (Let x e t)  -> (flip (Let x) t <$> eval e) <|> Just (subst e x t)
  (Case e pms) -> (flip Case pms <$> eval e) <|>
                  case e of
                      Con c args -> case find (\(Pat c' _ :=> _) -> c == c') pms of
                            Just (Pat _ an :=> t) -> 
                                let newNames = getFreeName (getFree e ++ (getFree t \\ an)) an
                                    t' = foldl (\t (from, to) -> rename from to t) t (zip an newNames)
                                in Just $ foldl (\t (n, v) -> subst v n t) t' (zip newNames args)
                            Nothing -> error $ "Invalid pattern-matching :" ++ c ++ "(...), pm = " ++ concat ((\(Pat c _ :=>b) -> c ++ ", ") <$> pms)
                      _ -> Nothing
  _            -> Nothing
  where
      eval = evalExpr1 evalContext defines
      evalArguments (x:xs) = ((:xs) <$> eval x) <|> ((x:) <$> evalArguments xs)
      evalArguments []     = Nothing
      tryGetValue (Val v) = Just v
      tryGetValue _       = Nothing
