module Generalization where

import Lang
import Utils
import Eval
import Data.Maybe
import Data.List
import Data.Bool
import Control.Applicative
import Instances

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
                                                in case uniteSubsts args subs1 subs2 of
                                                    (args, sub1, sub2) -> (funcCallToApp (h, args), sub1, sub2)
    _                                        -> let frees = getFree (a :@ b)
                                                    v = head $ filter (`notElem` frees) ['v' : show i | i <- [1..]]
                                                in (Var v, [v := a], [v := b])
    where
      getAllVariables s = domain s ++ concatMap getFree (range s)

      uniteSubsts :: [Term val bf bp] -> [Substitution (Term val bf bp)]  -> [Substitution (Term val bf bp)] -> ([Term val bf bp], Substitution (Term val bf bp), Substitution (Term val bf bp))
      uniteSubsts args subs1 subs2 = foldr (\((s1, s2), (arg, index)) (resArgs, resS1, resS2) -> 
              let frees = getFree arg `intersect` (domain s1 ++ domain s2)
                  banned = frees ++ (getAllVariables $ s1 ++ s2 ++ resS1 ++ resS2)
                  newFrees = take (length frees) $ filter (`notElem` banned) ["arg" ++ show index ++ "_" ++ show i | i <- [1..]]
                  renaming = zip frees newFrees
                  getNewSubs = foldr (\(x := y) s -> case lookup x renaming of 
                                                            Just x' -> (x' := subst (Var x') x y) : s
                                                            Nothing -> (x := y) : s) [] 
                  (newS1, newS2) = (getNewSubs s1, getNewSubs s2)
                  newArg = foldr (\(from, to) -> subst (Var to) from) arg renaming
              in (newArg : resArgs, newS1 ++ resS1, newS2 ++ resS2)
            ) ([], [], []) (zip (zip subs1 subs2) (zip args [1..]))


-- General subexpression rule
subExprRule :: (Eq val, Eq bf, Eq bp) => Generalization (Term val bf bp) -> Generalization (Term val bf bp)
subExprRule (e, [], []) = (e, [], [])
subExprRule (term, s1, s2) = case applySubs term (toMap s1) (toMap s2) of
                                (term, s1, s2) -> (term, toSubs s1, toSubs s2)
    where
        toMap subs = (\(n1 := e : subs) -> (e, n1 : map (\(n := _) -> n) subs))
                            <$> groupBy (\(_ := e) (_ := e') -> e == e') subs

        merge e vs vs' = case vs `intersect` vs' of
                          x : y : _ -> let (ne, nvs, nvs') = (subst (Var y) x e, delete x vs, delete x vs')
                                       in (merge ne nvs nvs') <|> Just (ne, nvs, nvs')
                          _         -> Nothing

        applySubsStep term subs1 subs2 = let allCombinations = do
                                                (i, (e , n1)) <- zip [1..] subs1
                                                (j, (e', n2)) <- zip [1..] subs2
                                                Just (term, n1, n2) <- return $ merge term n1 n2
                                                let s1 = take (i - 1) subs1 ++ ((e , n1) : drop i subs1)
                                                let s2 = take (j - 1) subs2 ++ ((e', n2) : drop j subs2)
                                                return (term, s1, s2)
                                         in listToMaybe allCombinations

        applySubs t s1 s2 = fromMaybe (t, s1, s2) (applySubsStep t s1 s2)

        toSubs = foldl (\s1 (e, n1) -> (s1 ++ map (:= e) n1)) []


assertDomainIsCoorect :: Substitution term -> Substitution term
assertDomainIsCoorect subs = if isSet $ domain subs then subs else error "Invalid substitution format"

isRenaming :: (Eq val, Eq bf, Eq bp) => Substitution (Term val bf bp) -> Bool
isRenaming x = let rng = range $ assertDomainIsCoorect x
               in all isVar rng && isSet rng
   where
      isVar (Var _) = True
      isVar _       = False


simplifyRenaming :: (Eq val, Eq bf, Eq bp) => Generalization (Term val bf bp) -> Generalization (Term val bf bp)
simplifyRenaming g@(e, s1, s2) = if isRenaming s1
        then let (e', s2') = foldr (\(x := e) (term, s2) -> 
                                      let Just (_ := Var x') = find (\(z := _) -> z == x) s1 
                                      in (subst (Var x') x term, (x' := e) : s2)
                                    ) (e, []) s2 
          
             in (e', [], s2')
        else g

-- Сlosest generalization operator
(>*<) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Generalization (Term val bf bp)
t1 >*< t2 = simplifyRenaming $ subExprRule (t1 >*<. t2)


