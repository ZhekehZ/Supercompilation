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
import Text.PrettyPrint

data Meta val bf bp = Regular
                    | MetaUp Int !(Substitution (Term val bf bp))
                    | MetaSplit (Term val bf bp) ![(Name, [Name])]
                    | MetaFun [Name]
                    | MetaLet

data Node val bf bp = Node { getTerm    :: !(Term val bf bp)
                           , getMeta    :: !(Meta val bf bp)
                           }


setMeta :: Meta val bf bp -> TreeIterator (Node val bf bp) -> TreeIterator (Node val bf bp)
setMeta meta iter = let (Node term _) = getCurrent iter in pSet (Node term meta) iter 


buildTreeStep :: (Eq val, Eq bf, Eq bp) => EvalContext val bf bp -> [Definition val bf bp] -> TreeIterator (Node val bf bp) ->
                                Either (TreeIterator (Node val bf bp)) (TreeIterator (Node val bf bp))
buildTreeStep ec defs ptree = 
    let tryEval = evalExpr ec defs e in
    if isNF tryEval then tryNext (pSet (Node tryEval Regular) ptree) else -- NF
    case renaming of
        Just (i, e', e'iter, path, _) -> -- RENAMING
            let Just ren = getRenaming e' e
                eiter = applyPath path $ setMeta (MetaFun (getFree e')) e'iter
            in  tryNext $ pSet (Node e (MetaUp i ren)) eiter
        Nothing -> 
            case substs of 
            Just  (i, e', e'iter, path, (g, s1, s2)) -> -- SUBSTITUTION
                let en = foldr (\(x := t) e -> Let x t e) g s1
                in tryNext $ pSetChildren (tToTree <$> (range s1 ++ [g])) $ pSet (Node en MetaLet) ptree
            Nothing -> case coupling of 
                Just (i, e', e'iter, path, (g, s1, s2)) -> -- COUPLING
                    let e'n = foldr (\(x := t) e -> Let x t e) g s2
                    in tryNext $ pSetChildren (tToTree <$> (range s2 ++ [g])) $ pSet (Node e'n MetaLet) e'iter
                Nothing -> case evalRedex <$> evalExpr1 ec defs e of
                        Just e' -> tryNext $ pSetChildren [tToTree e'] ptree 
                        _ -> let (ctx, red, mcon) = decompose e -- DRIVING
                             in case mcon of -- REDEX
                                Nothing -> case evalExpr1 ec defs red of
                                    Just red' -> Right $ pSet (Node (fillHole ctx red') Regular) ptree
                                    Nothing   -> tryNext ptree
                                Just conss -> let fill c args = case red of 
                                                                    Var v -> subst (Con c (map Var args)) v e -- EXTRA: REPLACE ALL OCCURRENCES
                                                                    _     -> fillCaseHole ctx c
                                                  (impl, names) = unzip $ (\(c, args) -> (fill c args, (c, args))) <$> conss
                                              in tryNext $ setMeta (MetaSplit e names) $ pSetChildren (tToTree <$> (red : impl)) ptree
    where
        tryNext it = maybe (Left it) Right (pNext it)
        haveNextChild = not . null . getRightChildren
        e = iToTerm ptree
        iToTerm = getTerm . getCurrent
        tToTree t = Branch (Node t Regular) []

        memo = unfoldr (\(it, i, path) -> flip fmap (pParent it) $ \(iter, ch) -> 
                            let path' = ch : path
                                term = iToTerm iter
                            in ((i, term, iter, path', e >*< term), (iter, i + 1, path'))
                       ) (ptree, 1, [])

        renaming = find (\(_,e',_,_,_) -> isRenaming e' e) memo
        substs   = find (\(_,e',_,_,_) -> isSubst    e' e) memo
        coupling = find (\(_,e',_,_,_) ->       e' <|.. e) memo

        applyPath = flip (foldl pChild)

        isSubst e1 e2 = case e1 >*< e2 of (e, s1, s2) -> all (not . isCon . rng) s2 && isRenaming e1 e && isRenaming e e1

        evalRedex r = maybe r evalRedex (evalRedex1 r)
        evalRedex1 red@((_ :-> _) :@ _) = evalExpr1 ec defs red
        evalRedex1 (a :@ b) = (:@ b) <$> evalRedex1 a
        evalRedex1 _ = Nothing


buildProgramTree ec (Program defs entry) = pToTree $ buildTree ec defs (PTree (Node (lookupFun defs entry) Regular) [] [] [])
    where
        buildTree dc defs ptree = either id (buildTree dc defs) (buildTreeStep dc defs ptree)



buildProgramTreeN ec (Program defs entry) n = pToTree $ buildTreeN ec defs (PTree (Node (lookupFun defs entry) Regular) [] [] []) n
    where
        buildTreeN dc defs ptree 0 = ptree
        buildTreeN dc defs ptree n = case buildTreeStep dc defs ptree of Left x -> x
                                                                         Right x -> buildTreeN dc defs x (n - 1)



instance Show x => Show (Tree x) where
    showsPrec p (Branch x xs) = showString (replicate (p * 2) ' ') . showsPrec p x
                    . foldl (\pr x -> pr . showChar '\n' . showsPrec (p + 1) x) id xs

instance (Show val, Show bf, Show bp) => Show (Meta val bf bp) where
    show meta = case meta of 
        Regular      -> "Regular"
        MetaUp i sub -> "Fold (" ++ show i ++ " up): " ++ show (map (\(x := y) -> text $ x ++ " := " ++ showOneLine y) sub)
        MetaSplit term cases -> "Split " ++ showOneLine term ++ ", cases: " ++ show (map fst cases)
        MetaFun args -> "Function with args: " ++ show args
        MetaLet -> "Let"
        where 
            showOneLine :: (Show val, Show bf, Show bp) => Term val bf bp -> String
            showOneLine = shortVersion . renderStyle (Style OneLineMode 0 0) . prettyPrintTerm False 0
            shortVersion s = if length s > 40 then '`' : take 40 s ++ "...`" else s 

instance (Show val, Show bf, Show bp) => Show (Node val bf bp) where
    showsPrec p (Node term meta) = let offset = replicate (p * 2) ' '
                                       idx = show p
                                       istr = replicate (4 - length idx) ' ' ++ idx
                                   in showString ("    ┌ Node\n" ++ offset ++ istr ++ "│ TERM:\n") 
                                      . showString (shift (p * 2 + 4) (show $ prettyPrintTerm False 0 term))
                                      . showString ("\n" ++ offset ++ "    └ META = ") . shows meta
        where shift p = show . vcat . fmap (text . ((replicate p ' ' ++ "│    ") ++)) . lines
