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

pParent :: ProcessTree x -> Maybe (ProcessTree x, Int)
pParent (PTree x l r []) = Nothing
pParent (PTree x l r ((p, pl, pr) : ps)) = Just $ (PTree p pl (Branch x (l ++ r) : pr) ps, length l)

pChild :: ProcessTree x -> Int -> ProcessTree x
pChild (PTree p pl (Branch x l_r : pr) ps) i = PTree x (take i l_r) (drop i l_r) ((p, pl, pr) : ps)

pParentN :: ProcessTree x -> Int -> Maybe (ProcessTree x, [Int])
pParentN = pParentN' []
    where
    pParentN' acc node 0 = Just (node, acc)
    pParentN' acc node n = maybe Nothing (\(parent, index) -> pParentN' (index:acc) parent (n - 1)) (pParent node)

pToTree :: ProcessTree x -> Tree x
pToTree pt@(PTree x l r _) = maybe (Branch x (l ++ r)) pToTree (fst <$> pParent pt)

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
            else case renaming of
                Just (i, term, parent, path, _, _, subs) ->
                        let newParent = pSet (Node term (Just $ MetaFun (getFree term))) parent
                            newNode = applyPath path $ newParent
                            newTree = pSetChildren [] $ pSet (Node e (Just $ MetaUp i subs)) newNode
                        in maybe (Left newTree) Right (pNext newTree)
                Nothing -> case coupling of
                    Just (_, _, parent, _, t, s, _) ->
                        let newTerm = foldr (\(x := t) e -> Let x t e) t s
                            children = flip Branch [] <$> ((flip Node Nothing <$> range s) ++ [Node t (Just $ MetaFun (getFree t))])
                            newTree = pSetChildren children $ pSet (Node newTerm (Just MetaLet)) parent
                        in maybe (Left newTree) Right (pNext newTree)
                    Nothing -> case evalExpr1 ec defs e of
                        Just x -> let newTree = pSetChildren [Branch (Node x Nothing) []] ptree
                                  in maybe (Left newTree) Right (pNext newTree)
                        Nothing -> let (ctx, term) = decompose e
                                       cases = getCaseVariants ctx
                                       meta = case cases of { [] -> Nothing; _ -> Just (MetaSplit term cases) }
                                       splitNode = pSet (Node e meta) ptree
                                       getNames n = take n ["a" ++ show i | i <- [1..]] \\ getFree e
                                       casesImpl = uncurry Con . fmap (fmap Var) <$> cases
                                       children = flip Branch [] . flip Node Nothing . fillHole ctx <$> casesImpl
                                       newTree = pSetChildren children splitNode
                                   in maybe (Left newTree) Right (pNext newTree)
    where
        iToTerm = getTerm . getCurrent
        nextOrThis = maybe (Left ptree) Right . pNext
        e = iToTerm ptree

        memo = [(i, term, iter, path, gener, subs1, subs2)
               | i <- [1 .. length (getParents ptree)]
               , let Just (iter, path) = pParentN ptree i
               , let term = iToTerm iter
               , let (gener, subs1, subs2) = term >*< iToTerm ptree
               ]

        renaming = find (\(_, _, _, _, _, subs1, _) -> null subs1) memo
        coupling = find (\(_, e', _, _, _, _, _) -> e' <|.. e) memo

--         applyPath :: [Int] -> ProcessTree (Node val bf bp) -> ProcessTree (Node val bf bp)
        applyPath []     node = node
        applyPath (x:xs) node = applyPath xs $ pChild node x


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


tt = Branch 1
     [ Branch 2
       [ Branch 3 []
       , Branch 4 []
       ]
     , Branch 5
       [ Branch 6 []
       , Branch 7 []
       ]
     ]