module Generalization where

import Lang
import Utils
import Eval
import Data.Maybe
import Data.List
import Data.Bool
import Control.Applicative


data SingleSub term = Name := term deriving Show
type Substitution term = [SingleSub term]
type Generalization term = (term, Substitution term, Substitution term)

domain :: Substitution term -> [Name]
domain = map (\(x := _) -> x)

range :: Substitution term -> [term]
range = map (\(_ := x) -> x)

applySubstitution :: Substitution (Term val bf bp) -> Term val bf bp -> Term val bf bp
applySubstitution subs term = foldl (\term (x := e) -> subst e x term) term subs

-- Common functor rule
(>*<.) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Generalization (Term val bf bp)
v >*<. v'@(Var _) | v == v' = (v, [], [])
a >*<. b = case (appAsFuncCall a, appAsFuncCall b) of
    (Just (h, xs), Just (h', xs')) | h == h' -> let (args, subs1, subs2) = unzip3 (uncurry (>*<.) <$> zip xs xs')
                                                in case uniteSubsts args subs1 of
                                                    (args, subs1) -> case uniteSubsts args subs2 of
                                                      (args, subs2) -> (funcCallToApp (h, args), subs1, subs2)
    _                                        -> (Var "v", ["v" := a], ["v" := b])
    where
      uniteSubsts :: [Term val bf bp] -> [Substitution (Term val bf bp)]
                        -> ([Term val bf bp], Substitution (Term val bf bp))
      uniteSubsts args listOfSubs = foldl (\(args, resSubs) (arg, subs) ->
                                              case addAllSubst (funcCallToApp (Left "_", args)) arg subs of
                                                (arg, subs) -> (args ++ [arg], resSubs ++ subs)
                                          ) ([], []) (zip args listOfSubs)

      addAllSubst :: Term val bf bp -> Term val bf bp -> Substitution (Term val bf bp)
                            -> (Term val bf bp, Substitution (Term val bf bp))
      addAllSubst pCall newArg =
            foldl (\(newArg, subs) sub -> ((subs ++) . (:[])) <$> addSubst pCall newArg sub) (newArg, [])

      addSubst :: Term val bf bp -> Term val bf bp -> SingleSub (Term val bf bp)
                                    -> (Term val bf bp, SingleSub (Term val bf bp))
      addSubst pCall newArg (x := e) = let x' = foldl (\l n -> bool l (l ++ "'") (l == n)) x (getFree pCall)
                                       in (renameToIn x x' newArg, x' := renameToIn x x' e)


-- General subexpression rule
subExprRule :: (Eq val, Eq bf, Eq bp) => Generalization (Term val bf bp) -> Generalization (Term val bf bp)
subExprRule (e, [], []) = (e, [], [])
subExprRule (term, s1, s2) = case applySubs term (toMap s1) (toMap s2) of
                                (term, s1, s2) -> (term, toSubs s1, toSubs s2)
    where
        toMap subs = (\(n1 := e : subs) -> (e, n1 : map (\(n := _) -> n) subs))
                            <$> groupBy (\(_ := e) (_ := e') -> e == e') subs

        merge e vs vs' = case vs `intersect` vs' of
                          x : y : _ -> let (ne, nvs, nvs') = (renameToIn x y e, delete x vs, delete x vs')
                                       in (merge ne nvs nvs') <|> Just (ne, nvs, nvs')
                          _         -> Nothing

        applySubsStep term subs1 subs2 = let allCombinations = do
                                                (i, (e , n1)) <- zip [1..] subs1
                                                (j, (e', n2)) <- zip [1..] subs2
                                                Just (term, n1, n2) <- return $ merge term n1 n2
                                                let s1 = take (i - 1) subs1 ++ ((e , n1) : drop i subs1)
                                                let s2 = take (j - 1) subs2 ++ ((e', n2) : drop j subs2)
                                                return (term, s1, s2)
                                         in case allCombinations of { [] -> Nothing; x : _ -> Just x }

        applySubs t s1 s2 = fromMaybe (t, s1, s2) (applySubsStep t s1 s2)

        toSubs = foldl (\s1 (e, n1) -> (s1 ++ map (:= e) n1)) []


assertDomainIsCoorect :: Substitution term -> Substitution term
assertDomainIsCoorect subs = case isSet $ domain subs of
                            True -> subs
                            False -> error "Invalid substitution format"


isRenaming :: (Eq val, Eq bf, Eq bp) => Substitution (Term val bf bp) -> Bool
isRenaming x = let rng = range $ assertDomainIsCoorect x
               in all isVar rng && isSet rng
   where
      isVar (Var _) = True
      isVar _       = False


simplifyRenaming :: (Eq val, Eq bf, Eq bp) => Generalization (Term val bf bp) -> Generalization (Term val bf bp)
simplifyRenaming g@(e, s1, s2) = if isRenaming s1
        then (applySubstitution s1 e, [], [x := e | (x' := e) <- s2, let x = getVarName (findByDomain x' s1)])
        else g
    where
        findByDomain :: Name -> Substitution term -> term
        findByDomain name = (\(Just (_ := x)) -> x) . find (\(x := _) -> x == name)

-- Сlosest generalization operator
(>*<) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Generalization (Term val bf bp)
t1 >*< t2 = simplifyRenaming $ subExprRule (t1 >*<. t2)


