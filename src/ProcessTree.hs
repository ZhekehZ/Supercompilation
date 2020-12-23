module ProcessTree where

-- Multiway tree
data Tree x = Branch x [Tree x]

data TreeIterator x = PTree { getCurrent       :: x
                            , getLeftChildren  :: [Tree x]
                            , getRightChildren :: [Tree x]
                            , getParents       :: [(x, [Tree x], [Tree x])]
                            }

-- Inorder traverse
pNext :: TreeIterator x -> Maybe (TreeIterator x)
pNext (PTree t l [] [])                 = Nothing
pNext (PTree t l [] ((p, ln, rn) : ps)) = pNext $ PTree p (ln ++ [Branch t l]) rn ps
pNext (PTree t l (Branch x ch : r) p)   = Just $ PTree x [] ch ((t, l, r) : p)

-- Set value to current node
pSet :: x -> TreeIterator x -> TreeIterator x
pSet x pt = pt {getCurrent = x}

-- Set chidren to current node
pSetChildren :: [Tree x] -> TreeIterator x -> TreeIterator x
pSetChildren ns (PTree x l r p) = PTree x [] ns p

-- Get parent and current node index in child list
pParent :: TreeIterator x -> Maybe (TreeIterator x, Int)
pParent (PTree x l r []) = Nothing
pParent (PTree x l r ((p, pl, pr) : ps)) = Just $ (PTree p pl (Branch x (l ++ r) : pr) ps, length l)

-- Get i-th child
pChild :: TreeIterator x -> Int -> TreeIterator x
pChild (PTree p pl (Branch x l_r : pr) ps) i = PTree x (take i l_r) (drop i l_r) ((p, pl, pr) : ps)

-- Get n-th parent and path to it
pParentN :: TreeIterator x -> Int -> Maybe (TreeIterator x, [Int])
pParentN = pParentN' []
    where
    pParentN' acc node 0 = Just (node, acc)
    pParentN' acc node n = maybe Nothing (\(parent, index) -> pParentN' (index:acc) parent (n - 1)) (pParent node)

pToTree :: TreeIterator x -> Tree x
pToTree pt@(PTree x l r _) = maybe (Branch x (l ++ r)) pToTree (fst <$> pParent pt)