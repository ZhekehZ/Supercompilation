module Generalization where

import Lang
import Utils
import Eval
import Data.Maybe
import Data.List
import Data.Bool
import Control.Applicative
import Instances 

data SingleSub term = Name := term deriving (Eq, Show)
type Substitution term = [SingleSub term]

type Generalization term = (term, Substitution term, Substitution term)

domain :: Substitution term -> [Name]
domain = map (\(x := _) -> x)

range :: Substitution term -> [term]
range = map (\(_ := x) -> x)

dom :: SingleSub term -> Name
dom (x := y) = x

rng :: SingleSub term -> term
rng (x := y) = y

findByDom :: Substitution term -> Name -> Maybe term
findByDom s n = rng <$> find (\(x := _) -> x == n) s

applySubstitution :: Substitution (Term val bf bp) -> Term val bf bp -> Term val bf bp
applySubstitution subs term = foldl (\term (x := e) -> subst e x term) term subs

-- Common functor rule
(>*<.) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Generalization (Term val bf bp)
a >*<. b = snd $ go (getFree a ++ getFree b) a b
    where 
        go x v v' | v == v' = (x, (v, [], [])) 
        go banned t1 t2 = case (appAsFuncCall t1, appAsFuncCall t2) of
            (Just (h, xs), Just (h', xs')) | h == h' -> 
                case foldr (\(a1, a2) (banned, gens) -> (:gens) <$> go banned a1 a2) (banned, []) (zip xs xs') of
                  (banned, gens) -> let (args, s1s, s2s) = unzip3 gens
                                    in (banned, (funcCallToApp (h, args), concat s1s, concat s2s))
            _ -> let [v] = getFreeName banned ["v"] in (v :  banned, (Var v, [v := t1], [v := t2]))


-- General subexpression rule
subExprRule :: (Eq val, Eq bf, Eq bp) => Generalization (Term val bf bp) -> Generalization (Term val bf bp)
subExprRule (e, [], []) = (e, [], [])
subExprRule g@(term, s1, s2) = case find ((> 1) . length) sames of
                  Just (v : v' : _) -> subExprRule (rename v v' term, filter ((/= v) . dom) s1, filter ((/= v) . dom) s2)
                  Nothing -> g 
  where sames = map fst <$> groupBy (\x y -> snd x == snd y) [(v, (t1, t2)) | v := t1 <- s1, Just t2 <- return $ findByDom s2 v]


getRenaming :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Maybe (Substitution (Term val bf bp))
getRenaming t1 t2 = filterOutId . nub <$> case (t1, t2) of
    (Var v   , Var v'   ) -> Just [v := Var v']
    (Con c a1, Con c' a2) | c == c'
                          , Just ss <- traverse (uncurry getRenaming) (zip a1 a2) -> Just $ concat ss
    (x :@ y  , z :@ t   ) | Just s1 <- getRenaming x z
                          , Just s2 <- getRenaming y t -> Just $ s1 ++ s2
    (Fun f   , Fun f'   ) | f == f' -> Just []
    _ -> Nothing
    where filterOutId = filter (\(x := y) -> case y of { Var z -> x /= z; _ -> True})


isRenaming :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
isRenaming a b | Just x <- getRenaming a b = True
isRenaming _ _ = False

-- Сlosest generalization operator
(>*<) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Generalization (Term val bf bp)
t1 >*< t2 = subExprRule (t1 >*<. t2)


