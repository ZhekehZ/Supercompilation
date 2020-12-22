module Driving where

import Lang
import Utils
import Eval
import Generalization
import Decomposition
import Embedding
import Data.List

data Tree x = Branch x [Tree x]

instance Show x => Show (Tree x) where
  showsPrec p (Branch x xs) = showString (take (p * 2) $ repeat ' ') . shows x
                    . foldl (\pr x -> pr . showChar '\n' . showsPrec (p + 1) x) id xs

data ProcessTree x = PTree { getCurrent       :: x
                           , getLeftChildren  :: [Tree x]
                           , getRightChildren :: [Tree x]
                           , getParents       :: [(x, [Tree x], [Tree x])]
                           } deriving Show


pNext :: ProcessTree x -> Maybe (ProcessTree x)
pNext (PTree t l [] [])                 = Nothing
pNext (PTree t l [] ((p, ln, rn) : ps)) = pNext $ PTree p (ln ++ [Branch t l]) rn ps
pNext (PTree t l (Branch x ch : r) p)   = Just $ PTree x [] ch ((t, l, r) : p)

pSet :: x -> ProcessTree x -> ProcessTree x
pSet x pt = pt {getCurrent = x}

pSetChildren :: [Tree x] -> ProcessTree x -> ProcessTree x
pSetChildren ns (PTree x l r p) = PTree x [] ns p

pParent :: ProcessTree x -> Maybe (ProcessTree x)
pParent (PTree x l r []) = Nothing
pParent (PTree x l r ((p, pl, pr) : ps)) = Just $ PTree p pl (Branch x (l ++ r) : pr) ps

pParentN :: ProcessTree x -> Int -> Maybe (ProcessTree x)
pParentN node 0 = Just node
pParentN node n = maybe Nothing (flip pParentN (n - 1)) (pParent node)

pToTree :: ProcessTree x -> Tree x
pToTree pt@(PTree x l r _) = maybe (Branch x (l ++ r)) pToTree (pParent pt)

data Meta val bf bp = MetaUp Int (Substitution (Term val bf bp))
                    | MetaSplit (Term val bf bp) [(Name, [Name])]
                    | MetaFun [Name]
                    | MetaLet
                    deriving Show

data Node val bf bp = Node { getTerm :: Term val bf bp
                           , getMeta :: Maybe (Meta val bf bp)
                           } deriving Show



buildTreeStep :: (Show val, Show bf, Show bp, Eq val, Eq bf, Eq bp) => EvalContext val bf bp -> [Definition val bf bp] -> ProcessTree (Node val bf bp) ->
                                Either (ProcessTree (Node val bf bp)) (ProcessTree (Node val bf bp))
buildTreeStep ec defs ptree =
        if isObservable e
            then nextOrThis $ pSetChildren [] ptree
            else case getRenaming of
                Just (i, (q, w, x)) ->
                        let newTree = pSetChildren [] $ pSet (Node e (Just $ MetaUp i x)) ptree
                        in maybe (Left newTree) Right (pNext newTree)
                Nothing -> case getCoupling of
                    Just (i, _, (t, s, s2)) ->
                        let newTerm = foldr (\(x := t) e -> Let x t e) t s
                            children = (flip Branch []) . flip Node Nothing <$> (range s ++ [t])
                            Just parent = pParentN ptree i
                            newTree = pSetChildren children $ pSet (Node newTerm (Just MetaLet)) parent
                        in maybe (Left newTree) Right (pNext newTree)
                    Nothing -> case evalExpr1 ec defs e of
                        Just x -> let newTree = pSetChildren [Branch (Node x Nothing) []] ptree
                                  in maybe (Left newTree) Right (pNext newTree)
                        Nothing -> let (ctx, term) = decompose e
                                       cases = getCaseVariants ctx
                                       meta = case cases of { [] -> Nothing; _ -> Just (MetaSplit term cases) }
                                       splitNode = pSet (Node e meta) ptree
                                       getNames n = take n ["arg" ++ show i | i <- [1..]] \\ getFree e
                                       casesImpl = uncurry Con . fmap (fmap Var) <$> cases
                                       children = flip Branch [] . flip Node Nothing . fillHole ctx <$> casesImpl
                                       newTree = pSetChildren children splitNode
                                   in maybe (Left newTree) Right (pNext newTree)
    where
        nextOrThis = maybe (Left ptree) Right . pNext
        e = getTerm $ getCurrent ptree
        memoized = zip [1..] $ (\(x, _, _) -> getTerm x) <$> getParents ptree
        geners = fmap (>*< e) <$> memoized
        getRenaming = find (\(i, (_, x, _)) -> null x) geners
        getCoupling = case [(i, e', gen) | ((i, e'), (_, gen)) <- zip memoized geners, e' <|.. e] of
                            x : _ -> Just x
                            _     -> Nothing


buildProgramTree ec (Program defs entry) = pToTree $ buildTree ec defs (PTree (Node (lookupFun defs entry) Nothing) [] [] [])
    where
        buildTree ec defs ptree = either id (buildTree ec defs) $ buildTreeStep ec defs ptree



buildProgramTreeN ec (Program defs entry) n = pToTree $ either id id $ buildTreeSteps n ec defs (PTree (Node (lookupFun defs entry) Nothing) [] [] [])
    where
    buildTreeSteps 1 ec defs ptree = buildTreeStep ec defs ptree
    buildTreeSteps n ec defs ptree = case buildTreeStep ec defs ptree of
                                        Left x -> Left x
                                        Right y -> buildTreeSteps (n - 1) ec defs y
{-

let v = 1 in let v' = 0 in sum (squares (upto v arg0)) v'

     case case case Gt(v, arg0) of { True() => Nil(), False() => Cons(v, upto Plus(v, 1) arg0) } of { Nil() => Nil(), Cons("arg1", "arg2") => Cons(Mul(arg1, arg1), squares arg2) } of { Nil() => v', Cons("arg1'", "arg2'") => sum arg2' Plus(arg1', v') }

           True  => v'
           False => sum (squares (upto Plus(v, 1) arg0)) Plus(Mul(v, v), v')


-}
