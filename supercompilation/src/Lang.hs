module Lang where

type Name = String

data Term val bf bp
              = Val  { getValue                :: val
                     }
              | ValF { getBuiltinFunction      :: bf
                     , getArguments            :: [Term val bf bp]
                     }
              | ValP { getBuiltinPredicate     :: bp
                     , getArguments            :: [Term val bf bp]
                     }
              | Var  { getVarName              :: Name
                     }
              | Con  { getConstructor          :: Name
                     , getConstructorArguments :: [Term val bf bp]
                     }
              | Fun  { getFunction             :: Name
                     }
              | Term val bf bp  :@  Term val bf bp
              | Name            :-> Term val bf bp
              | Case { getCaseVariable         :: Term val bf bp
                     , getCases                :: PatternMatching val bf bp
                     }
              | Let  { getLetVarName           :: Name
                     , getLetValue             :: Term val bf bp
                     , getLetBody              :: Term val bf bp
                     }

infixr 4 :->
infixl 6 :@

type PatternMatching val bf bp = [PatternMatchingCase val bf bp]

data PatternMatchingCase val bf bp = Pattern :=> Term val bf bp deriving Show
infix 5 :=>

data Pattern = Pat { getPConstructor          :: Name
                   , getPConstructorVariables :: [Name]
                   } deriving (Show, Eq)

data Definition val bf bp = Def Name (Term val bf bp) deriving (Show, Eq)

data Program val bf bp = Program { getPrDefinitions :: [Definition val bf bp]
                                 , getPrEntryPoint  :: Name
                                 } deriving (Show, Eq)

type BuiltinFunctionEval val bf = bf -> [val] -> val
type BuiltinPredicateEval val bp = bp -> [val] -> Bool


renameToIn :: Name -> Name -> Term val bf bp -> Term val bf bp
renameToIn wh to term = if wh == to then term else case term of
    (Val v)         -> Val v
    (Fun f)         -> Fun f
    (Var x)         -> Var $ if x == wh then to else x
    (ValF f args)   -> ValF f (whRenameToInTo <$> args)
    (ValP f args)   -> ValP f (whRenameToInTo <$> args)
    (Con  f args)   -> Con  f (whRenameToInTo <$> args)
    (x :-> t)       -> if x == wh then x :-> t else x :-> whRenameToInTo t
    (a :@ b)        -> whRenameToInTo a :@ whRenameToInTo b
    (Case e pmc)    -> Case (whRenameToInTo e) (whRenameToInToPM <$> pmc)
    (Let x e t)     -> Let x (whRenameToInTo e) (if x == wh then t else whRenameToInTo t)
    where
        whRenameToInTo = wh `renameToIn` to
        whRenameToInToPM (pm@(Pat _ args) :=> t) = pm :=> (if wh `elem` args then t else whRenameToInTo t)


instance (Show val, Show bf, Show bp) => Show (Term val bf bp) where
    show t = case t of
        Val v -> show v
        ValF f as -> show f ++ "(" ++ case as of {[] -> ""; (a : as) -> foldl (\s a -> s ++ ", " ++ show a) (show a) as} ++ ")"
        ValP f as -> show f ++ "(" ++ case as of {[] -> ""; (a : as) -> foldl (\s a -> s ++ ", " ++ show a) (show a) as} ++ ")"
        Con f as -> f ++ "(" ++ case as of {[] -> ""; (a : as) -> foldl (\s a -> s ++ ", " ++ show a) (show a) as} ++ ")"
        Var v -> v
        Fun v -> v
        a :@ b -> "(" ++ show a ++ ") (" ++ show b ++ ")"
        a :-> b -> "\\" ++ a ++ " -> " ++ show b
        Case e cases -> "case " ++ show e ++ " of\n\t" ++ show cases
        Let x e e' -> "Let " ++ x ++ " = " ++ show e ++ " in " ++ show e'


instance (Eq val, Eq bf, Eq bp) => Eq (Term val bf bp) where
    Val x == Val x' = x == x'
    Var x == Var x' = x == x'
    Fun x == Fun x' = x == x'
    ValF f args == ValF f' args' = (f == f') && (args == args')
    ValP p args == ValP p' args' = (p == p') && (args == args')
    Con c args == Con c' args' = (c == c') && (args == args')
    (a :@ b) == (a' :@ b') = (a == a') && (b == b')
    (x :-> t) == (y :-> t') = renameToIn x y t == t'
    Case t cases == Case t' cases' = (t == t') && (cases == cases')
    Let x e t == Let y e' t' = (e == e') && ((x :-> t) == (y :-> t'))
    _ == _ = False

instance (Eq val, Eq bf, Eq bp) => Eq (PatternMatchingCase val bf bp) where
    (Pat c args :=> t) == (Pat c' args' :=> t') = (c == c')
            && (t == foldl (\t' (a', a) -> renameToIn a' a t') t' (zip args' args))
