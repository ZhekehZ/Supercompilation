module Driving where

import Lang
import Utils
import Eval
import Generalization
import Decomposition
import Embedding
import Data.List
import Data.Maybe
import ProcessTree

data Meta val bf bp = Regular
                    | MetaUp Int (Substitution (Term val bf bp))
                    | MetaSplit (Term val bf bp) [(Name, [Name])]
                    | MetaFun [Name]
                    | MetaLet
                    deriving Show

data Node val bf bp = Node { getTerm    :: Term val bf bp
                           , getMeta    :: Meta val bf bp
                           }


setMeta :: Meta val bf bp -> TreeIterator (Node val bf bp) -> TreeIterator (Node val bf bp)
setMeta meta iter = let (Node term _) = getCurrent iter in pSet (Node term meta) iter 


buildTreeStep :: (Eq val, Eq bf, Eq bp) => EvalContext val bf bp -> [Definition val bf bp] -> TreeIterator (Node val bf bp) ->
                                Either (TreeIterator (Node val bf bp)) (TreeIterator (Node val bf bp))
buildTreeStep ec defs ptree = 
    if isObservable e then tryNext ptree else
    case renaming of
        Just (i, e', e'iter, path, _, _, _) -> -- RENAMING
            let Just ren = getRenaming e' e
                eiter = applyPath path $ setMeta (MetaFun (getFree e')) e'iter
            in  tryNext $ pSet (Node e (MetaUp i ren)) eiter
        Nothing -> case substs of 
            Just  (i, e', e'iter, path, g, s1, s2) -> -- SUBSTITUTION
                        let en = foldr (\(x := t) e -> Let x t e) g s1
                        in tryNext $ pSet (Node en MetaLet) ptree
            Nothing -> case coupling of 
                Just (i, e', e'iter, path, g, s1, s2) -> -- COUPLING
                    let e'n = foldr (\(x := t) e -> Let x t e) g s2
                    in tryNext $ pSetChildren [tToTree g] $ pSet (Node e'n MetaLet) e'iter
                Nothing -> case e of
                        Var x -> tryNext ptree -- VARIABLE (extra case)
                        _ -> let (ctx, red) = decompose e -- DRIVING
                                 e'' = fillHole ctx <$> evalExpr1 ec defs red
                                 eredt = flip Node Regular <$> e''
                             in case red of
                                 Fun _            -> tryNext $ pSetChildren [tToTree $ fromJust e''] $ ptree 
                                 (_ :-> _) :@ _   -> {-buildTreeStep ec defs-} Right $ pSet (fromJust eredt) ptree  -- uncomment and remove `Right`
                                 Case Con{} _     -> {-buildTreeStep ec defs-} Right $ pSet (fromJust eredt) ptree  --  to skip intermediate steps
                                 Case (Var v) pm  -> -- (extra case : find and replace all occurrences)
                                     let (caseImpls, caseNames) = unzip $ (\(Pat c ns :=> _) -> (subst (Con c (map Var ns)) v e, (c, ns))) <$> pm 
                                     in tryNext $ setMeta (MetaSplit e caseNames) $ pSetChildren (tToTree <$> (Var v : caseImpls)) ptree
                                 Case ce pm       -> 
                                     let (caseImpls, caseNames) = unzip $ (\(Pat c ns :=> _) -> (fillHole ctx (Con c (map Var ns)), (c, ns))) <$> pm 
                                     in tryNext $ setMeta (MetaSplit e caseNames) $ pSetChildren (tToTree <$> (ce : caseImpls)) ptree
    where
        tryNext it = maybe (Left it) Right (pNext it)
        haveNextChild = not . null . getRightChildren
        e = iToTerm ptree
        iToTerm = getTerm . getCurrent

        tToTree t = Branch (Node t Regular) []

        memo = [(i, term, iter, path, gener, subs1, subs2)
               | i <- [1 .. length (getParents ptree)]
               , let Just (iter, path) = pParentN ptree i
               , let term = iToTerm iter
               , let (gener, subs1, subs2) = e >*< term
               ]

        renaming = find (\(_,e',_,_,_,_,_) -> isRenaming e' e) memo
        substs = find (\(_,e',_,_,_,_,_) -> isSubst e' e) memo
        coupling = find (\(_,e',_,_,_,_,_) -> e' <|.. e) memo

        applyPath []     node = node
        applyPath (x:xs) node = applyPath xs $ pChild node x
        isSubst e1 e2 = case e1 >*< e2 of (e, s1, s2) -> all (not . isCon . rng) s2 && isRenaming e1 e && isRenaming e e1


buildProgramTree ec (Program defs entry) = pToTree $ buildTree ec defs (PTree (Node (lookupFun defs entry) Regular) [] [] [])
    where
        buildTree dc defs ptree = either id (buildTree dc defs) (buildTreeStep dc defs ptree)



buildProgramTreeN ec (Program defs entry) n = pToTree $ buildTreeN ec defs (PTree (Node (lookupFun defs entry) Regular) [] [] []) n
    where
        buildTreeN dc defs ptree 0 = ptree
        buildTreeN dc defs ptree n = case buildTreeStep dc defs ptree of Left x -> x
                                                                         Right x -> buildTreeN dc defs x (n - 1)



instance Show x => Show (Tree x) where
    showsPrec p (Branch x xs) = showString (take (p * 2) $ repeat ' ') . showsPrec p x
                    . foldl (\pr x -> pr . showChar '\n' . showsPrec (p + 1) x) id xs

instance (Show val, Show bf, Show bp) => Show (Node val bf bp) where
    showsPrec p (Node term meta) = let offset = take (p * 2) $ repeat ' '
                                       idx = show p
                                       istr = take (4 - length idx) (repeat ' ') ++ idx
                                   in showString ("    | Node\n" ++ offset ++ istr ++ "| TERM = ") . shows term 
                                      . showString ("\n" ++ offset ++ "    | META = ") . shows meta


{-

fun1 = \s -> case s of { 
            Nil => False, 
            Cons(a1, a2) => case a1 of { 
                A => True, 
                B => case s of { 
                    Nil => s, 
                    Cons(a1, a2') => fun1 a2' 
                }, 
                C => case s of { 
                    Nil => s, 
                    Cons(a1, a2') => fun1 a2' 
                } 
            } 
        }

fun1 = \s -> case s of { 
            Nil => False, 
            Cons(a1, a2) => case a1 of { 
                A => case a2 of { 
                    Nil => False, 
                    Cons(a1, a2') => case a1 of { 
                        A => case s of { 
                            Cons(a1, a2') => fun1 a2' 
                        }, 
                        B => True, 
                        C => case s of { 
                            Cons(a1, a2') => fun1 a2' 
                        } 
                    } 
                }, 
                B => case s of { 
                    Cons(a1, a2') => fun1 a2' 
                }, 
                C => case s of { 
                    Cons(a1, a2') => fun1 a2' 
                } 
            }
        }

-}