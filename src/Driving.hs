module Driving where

import Lang
import Utils
import Eval
import Generalization
import Decomposition
import Embedding
import Data.List
import ProcessTree

data Meta val bf bp = Regular
                    | MetaUp Int (Substitution (Term val bf bp))
                    | MetaSplit (Term val bf bp) [(Name, [Name])]
                    | MetaFun [Name]
                    | MetaLet
                    deriving Show

data Node val bf bp = Node { getTerm    :: Term val bf bp
                           , getMeta    :: Meta val bf bp
                           } deriving Show


buildTreeStep :: (Eq val, Eq bf, Eq bp) =>
    EvalContext val bf bp -> [Definition val bf bp] -> TreeIterator (Node val bf bp) ->
                                Either (TreeIterator (Node val bf bp)) (TreeIterator (Node val bf bp))
buildTreeStep ec defs ptree =
        case evalExpr ec defs e of 
            x | isObservable x -> let Just newTree = pNext $ pSetChildren [Branch (Node x Regular) []] $ ptree in maybe (Left newTree) Right (pNext newTree)
            _                  -> case renaming of
                Just (i, term, parent, path, _, _, subs) ->
                        let newParent = pSet (Node term (MetaFun (getFree term))) parent
                            newNode = applyPath path $ newParent
                            newTree = pSetChildren [] $ pSet (Node e (MetaUp i subs)) newNode
                        in maybe (Left newTree) Right (pNext newTree)
                Nothing -> case coupling of
                    Just (_, _, parent, _, t, s, _) ->
                        let newTerm = foldr (\(x := t) e -> Let x t e) t s
                            children = flip Branch [] <$> ((flip Node Regular <$> range s) ++ [Node t (MetaFun (getFree t))])
                            newTree = pSetChildren children $ pSet (Node newTerm MetaLet) parent
                        in maybe (Left newTree) Right (pNext newTree)
                    Nothing -> case evalExpr1 ec defs e of
                        Just x -> let newTree = pSetChildren [Branch (Node x Regular) []] ptree
                                  in maybe (Left newTree) Right (pNext newTree)
                        Nothing -> let (ctx, term) = decompose e
                                       cases = getCaseVariants ctx
                                       meta = case cases of { [] -> Regular; _ -> MetaSplit term cases }
                                       splitNode = pSet (Node e meta) ptree
                                       getNames n = take n ["a" ++ show i | i <- [1..]] \\ getFree e
                                       casesImpl = uncurry Con . fmap (fmap Var) <$> cases
                                       children = flip Branch [] . (\x -> Node x Regular) . fillHole ctx <$> casesImpl
                                       newTree = pSetChildren children splitNode
                                   in maybe (Left newTree) Right (pNext newTree)
    where
        iToTerm = getTerm . getCurrent
        e = iToTerm ptree
        nextOrThis = maybe (Left ptree) Right . pNext

        memo = [(i, term, iter, path, gener, subs1, subs2)
               | i <- [1 .. length (getParents ptree)]
               , let Just (iter, path) = pParentN ptree i
               , let term = iToTerm iter
               , let (gener, subs1, subs2) = term >*< iToTerm ptree
               ]

        renaming = find (\(_, _, _, _, _, subs1, _) -> null subs1) memo
        coupling = find (\(_, e', _, _, _, _, _) -> e' <|.. e) memo

        applyPath []     node = node
        applyPath (x:xs) node = applyPath xs $ pChild node x


buildProgramTree ec (Program defs entry) = pToTree $ buildTree ec defs (PTree (Node (lookupFun defs entry) Regular) [] [] [])
    where
        buildTree dc defs ptree = either id (buildTree dc defs) (buildTreeStep dc defs ptree)



buildProgramTreeN ec (Program defs entry) n = pToTree $ buildTreeN ec defs (PTree (Node (lookupFun defs entry) Regular) [] [] []) n
    where
        buildTreeN dc defs ptree 0 = ptree
        buildTreeN dc defs ptree n = case buildTreeStep dc defs ptree of Left x -> x
                                                                         Right x -> buildTreeN dc defs x (n - 1)