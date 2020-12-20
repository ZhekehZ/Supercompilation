module Driving where

import Lang
import Utils
import Data.List
import Decomposition
import Eval

data Tree x = Branch x [Tree x]

instance Show x => Show (Tree x) where
  showsPrec p (Branch x xs) = showString (take (p * 2) $ repeat ' ') . shows x
                    . foldl (\pr x -> pr . showChar '\n' . showsPrec (p + 1) x) id xs

data ProcessTree x = PTree { getCurrent       :: x
                           , getLeftChildren  :: [Tree x]
                           , getRightChildren :: [Tree x]
                           , getParents       :: [(x, [Tree x], [Tree x])]
                           } deriving Show

zipper :: Tree x -> ProcessTree x
zipper (Branch x xs) = PTree x [] xs []


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


data Node val bf bp = Node { getTerm :: Term val bf bp }

buildTreeStep :: EvalContext val bf bp -> [Definition val bf bp] -> ProcessTree (Node val bf bp) -> Maybe (ProcessTree (Node val bf bp))
buildTreeStep ec defs ptree =
    if isObservable (getTerm $ getCurrent ptree)
    then pNext (pSetChildren [] ptree)
    else undefined



asJust (Just x) = x

getAll :: a -> (a -> Maybe a) -> [a]
getAll a f = case f a of { Nothing -> [a]; Just x -> a : getAll x f }

t = Branch 1
        [ Branch 2 [ Branch 3 [], Branch 4 [] ]
        , Branch 5 [ Branch 6 [], Branch 7 [], Branch 8 [] ]
        , Branch 9 []
        ]