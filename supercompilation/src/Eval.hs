module Eval where

import Lang
import Data.List
import Control.Applicative
import Data.Bool
import Data.Maybe

data EvalContext val bf bp = EC [(Name, val)] (BuiltinFunctionEval val bf) (BuiltinPredicateEval val bp)

evalProgram :: Program val bf bp -> EvalContext val bf bp -> Term val bf bp
evalProgram (Program def entry) context = evalExpr context def (lookupFun def entry)

lookupFun :: [Definition val bf bp] -> Name -> Term val bf bp
lookupFun (Def defName expr : def) name | name == defName = expr
                                        | otherwise       = lookupFun def name
lookupFun [] name = error $ "Invalid function name: " ++ name

getFree :: Term val bf bp -> [Name]
getFree term = case term of
    ValF _ args -> args >>= getFree
    ValP _ args -> args >>= getFree
    Con  _ args -> args >>= getFree
    x :-> t     -> filter (/= x) (getFree t)
    a :@ b      -> getFree a `union` getFree b
    Case e p    -> getFree e `union` (p >>= getFreePM)
    Let x e t   -> getFree e `union` filter (/= x) (getFree t)
    Var x       -> [x]
    _           -> []
    where
        getFreePM ((Pat _ args) :=> t) = getFree t \\ args




subst :: Term val bf bp -> Name -> Term val bf bp -> Term val bf bp
subst new name expr = case expr of
    Val v       -> Val v
    Fun f       -> Fun f
    Var x       -> if x == name then new else Var x
    ValF f args -> ValF f (substThis <$> args)
    ValP p args -> ValP p (substThis <$> args)
    Con  c args -> Con  c (substThis <$> args)
    x :-> t     -> let x' = assertFree x (getFree t \\ [x]) in x' :-> substThis (renameToIn x x' t)
    Let x e t   -> let x' = assertFree x (getFree t \\ [x]) in Let x' (substThis e) (substThis (renameToIn x x' t))
    e1 :@ e2    -> substThis e1 :@ substThis e2
    Case e pmc  -> Case (substThis e) (substThisPM <$> pmc)
    where
        freeInNew = getFree new
        assertFree x free = if x `elem` freeInNew || x `elem` free then assertFree (x ++ "'") free else x
        substThis = subst new name

        substThisPM (Pat c args :=> t) =
            let uniArgs = (\(_, n) -> "arg" ++ show n) <$> zip args [1 :: Int .. ]
                freArgs = (\x -> assertFree x (getFree t)) <$> uniArgs
            in Pat c freArgs :=> substThis (foldl (\term (wh, to) -> renameToIn wh to term) t (zip args freArgs))



lookupPM :: Name -> PatternMatching val bf bp -> Maybe (PatternMatchingCase val bf bp)
lookupPM c (pmc@(Pat c' _ :=> _) : pms) | c == c'   = Just pmc
                                        | otherwise = lookupPM c pms
lookupPM _ _ = Nothing


evalExprStep :: EvalContext val bf bp -> [Definition val bf bp] -> Term val bf bp -> Maybe (Term val bf bp)
evalExprStep evalContext@(EC context bfEval bpEval) defines expression = case expression of
    ValF f args  -> (ValF f <$> evalArguments args) <|> Just (Val $ bfEval f (getValue <$> args))
    ValP p args  -> (ValP p <$> evalArguments args) <|> Just (Con (show $ bpEval p (getValue <$> args)) [])
    Var v        -> Val <$> lookup v context
    Fun f        -> Just $ lookupFun defines f
    ((x:->e):@t) -> Just $ subst t x e
    (e1:@e2)     -> (:@e2) <$> eval e1
    (Let x e t)  -> (flip (Let x) t <$> eval e) <|> Just (subst e x t)
    (Case e pms) -> (flip Case pms <$> eval e) <|>
                    case e of
                        Con c args -> case lookupPM c pms of
                            Just (Pat _ argNames :=> t) ->
                                Just $ foldl (\term (name, value) -> subst value name term) t (zip argNames args)
                            Nothing -> error "Invalid pattern-matching"
                        _ -> Nothing
    _ -> Nothing

    where
        eval = evalExprStep evalContext defines
        evalArguments (x:xs) = ((:xs) <$> eval x) <|> ((x:) <$> evalArguments xs)
        evalArguments []     = Nothing


evalExpr :: EvalContext val bf bp -> [Definition val bf bp] -> Term val bf bp -> Term val bf bp
evalExpr context def t = maybe t (evalExpr context def) (evalExprStep context def t)









(<|) :: (Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
a <| b = (a <|. b) || (a <|.. b) || (a <|... b)

-- Variable
(<|.) :: Term val bf bp -> Term val bf bp -> Bool
Var _ <|. Var _ = True
_     <|. _     = False

-- Coupling
(<|..) :: (Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
f_e1_en <|.. f_e1_en' = case (appAsFuncCall f_e1_en, appAsFuncCall f_e1_en') of
    (Just (f, as), Just (f', as')) | f == f' -> all (uncurry (<|)) (zip as as')
    _                                        -> False

-- Diving
(<|...) :: (Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
e <|... h_e1_en = case appAsFuncCall h_e1_en of
    Just (_, as) -> any (e <|) as
    _            -> False


type FuncCall val bf bp = (Either Name (Either bf bp), [Term val bf bp])

appAsFuncCall :: Term val bf bp -> Maybe (FuncCall val bf bp)
appAsFuncCall (a :@ b)      = (\(name, args) -> (name, args ++ [b])) <$> appAsFuncCall a
appAsFuncCall (Fun f)       = Just (Left f, [])
appAsFuncCall (ValF f args) = Just (Right (Left f), args)
appAsFuncCall (ValP p args) = Just (Right (Right p), args)
appAsFuncCall _             = Nothing

funcCallToApp :: FuncCall val bf bp -> Term val bf bp
funcCallToApp (Left f, args) = foldl (:@) (Fun f) args
funcCallToApp (Right (Left f), args) = ValF f args
funcCallToApp (Right (Right p), args) = ValP p args


data SingleSub term = Name := term deriving Show
type Substitution term = [SingleSub term]
type Generalization term = (term, Substitution term, Substitution term)


(##.) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Generalization (Term val bf bp)
v ##. v'@(Var _) | v == v' = (v, [], [])
a ##. b = case (appAsFuncCall a, appAsFuncCall b) of
    (Just (h, as), Just (h', as')) | h == h' -> let (args, subs1, subs2) = unzip3 (uncurry (##.) <$> zip as as')
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


(##) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Generalization (Term val bf bp)
t1 ## t2 = subExprRule (t1 ##. t2)