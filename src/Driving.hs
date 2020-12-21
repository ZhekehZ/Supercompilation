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
pNext (PTree t l [] ((p, ln, rn) : ps)) = pNext $ PTree p (Branch t l : ln) rn ps
pNext (PTree t l (Branch x ch : r) p)   = Just $ PTree x [] ch ((t, l, r) : p)

pPrev :: ProcessTree x -> Maybe (ProcessTree x)
pPrev (PTree t [] r [])                 = Nothing
pPrev (PTree t (Branch x [] : l) r p)   = Just $ PTree x [] [] ((t, l, r) : p)
pPrev (PTree t (Branch x ch : l) r p)   = pPrev $ PTree x ch [] ((t, l, r) : p)
pPrev (PTree t [] r ((p, [], rn) : ps)) = Just $ PTree p [] (Branch t r : rn) ps
pPrev (PTree t [] r ((p, ln, rn) : ps)) = pPrev $ PTree p ln (Branch t r : rn) ps

pSet :: x -> ProcessTree x -> ProcessTree x
pSet x pt = pt {getCurrent = x}

pSetChildren :: [Tree x] -> ProcessTree x -> ProcessTree x
pSetChildren ns (PTree x l r p) = PTree x [] ns p

pParent :: ProcessTree x -> Maybe (ProcessTree x)
pParent (PTree x l r []) = Nothing
pParent (PTree x l r ((p, pl, pr) : ps)) = Just $ PTree p pl (Branch x (reverse l ++ r) : pr) ps

pParentN :: ProcessTree x -> Int -> Maybe (ProcessTree x)
pParentN node 0 = Just node
pParentN node n = maybe Nothing (flip pParentN (n - 1)) (pParent node)

pToTree :: ProcessTree x -> Tree x
pToTree pt@(PTree x l r _) = maybe (Branch x (reverse l ++ r)) pToTree (pParent pt)

data Meta val bf bp = MetaUp Int (Substitution (Term val bf bp))
                    | MetaSplit (Term val bf bp) (Context val bf bp)
                    | MetaLet
                    deriving Show

data Node val bf bp = Node { getTerm :: Term val bf bp
                           , getMeta :: Maybe (Meta val bf bp)
                           } deriving Show



buildTreeStep :: (Eq val, Eq bf, Eq bp) => EvalContext val bf bp -> [Definition val bf bp] -> ProcessTree (Node val bf bp) ->
                                Either (ProcessTree (Node val bf bp)) (ProcessTree (Node val bf bp))
buildTreeStep ec defs ptree =
        if isObservable e
            then nextOrThis $ pSetChildren [] ptree
            else case getRenaming of
                Just (i, (_, _, x)) -> let newTree = pSetChildren [] $ pSet (Node e (Just $ MetaUp i x)) ptree
                                       in maybe (Left newTree) Right (pNext newTree)
                Nothing -> case getCoupling of
                    Just (i, _, (t, s, _)) ->
                        let newTerm = foldr (\(x := t) e -> Let x t e) t s
                            children = (flip Branch []) . flip Node Nothing <$> (range s ++ [t])
                            Just parent = pParentN ptree i
                            newTree = pSetChildren children $ pSet (Node newTerm (Just MetaLet)) parent
                        in maybe (Left newTree) Right (pNext newTree)
                    Nothing -> case evalExpr1 ec defs e of
                        Just x -> let newTree = pSetChildren [Branch (Node x Nothing) []] ptree
                                  in maybe (Left newTree) Right (pNext newTree)
                        Nothing -> let (ctx, term) = decompose e
                                       splitNode = pSet (Node e (Just (MetaSplit term ctx))) ptree
                                       cases = getCaseVariants ctx
                                       getNames n = take n ["arg" ++ show i | i <- [1..]] \\ getFree e
                                       casesImpl = [Con c args | (c, n) <- cases, let args = Var <$> getNames n]
                                       children = (flip Branch []) . flip Node Nothing . fillHole ctx <$> casesImpl
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


-- buildTree ec defs ptree =

-- squeezeTransitions

buildTreeSteps 1 ec defs ptree = buildTreeStep ec defs ptree
buildTreeSteps n ec defs ptree = case buildTreeStep ec defs ptree of
                                    Left x -> Left x
                                    Right y -> buildTreeSteps (n - 1) ec defs y


asJust (Just x) = x

getAll :: a -> (a -> Maybe a) -> [a]
getAll a f = case f a of { Nothing -> [a]; Just x -> a : getAll x f }

t = Branch 1
        [ Branch 2 [ Branch 3 [], Branch 4 [] ]
        , Branch 5 [ Branch 6 [], Branch 7 [], Branch 8 [] ]
        , Branch 9 []
        ]


{-

let v = 1 in let v' = 0 in sum (squares (upto v arg0)) v'

     case case case Gt(v, arg0) of { True() => Nil(), False() => Cons(v, upto Plus(v, 1) arg0) } of { Nil() => Nil(), Cons("arg1", "arg2") => Cons(Mul(arg1, arg1), squares arg2) } of { Nil() => v', Cons("arg1'", "arg2'") => sum arg2' Plus(arg1', v') }

           True  => v'
           False => sum (squares (upto Plus(v, 1) arg0)) Plus(Mul(v, v), v')


-}
