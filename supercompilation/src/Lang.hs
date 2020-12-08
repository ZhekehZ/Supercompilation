module Lang where

type Name = String

data Term val = Val  { getValue                :: val
                     }
              | ValF { getBuiltinFunction      :: [val] -> val
                     , getArguments            :: [Term val]
                     }
              | ValP { getBuiltinPredicate     :: [val] -> Bool
                     , getArguments            :: [Term val]
                     }
              | Var  { getVarName              :: Name
                     }
              | Con  { getConstructor          :: Name
                     , getConstructorArguments :: [Term val]
                     }
              | Fun  { getFunction             :: Name
                     }
              | Term val :@  Term val
              | Name     :-> Term val
              | Case { getCaseVariable         :: Term val
                     , getCases                :: PatternMatching val
                     }
              | Let  { getLetVarName           :: Name
                     , getLetValue             :: Term val
                     , getLetBody              :: Term val
                     }

infixr 4 :->
infixl 6 :@

type PatternMatching val = [PatternMatchingCase val]

data PatternMatchingCase val = Pattern :=> Term val deriving Show
infix 5 :=>

data Pattern = Pat { getPConstructor          :: Name
                   , getPConstructorVariables :: [Name]
                   } deriving Show

data Definition val = Def Name (Term val) deriving Show

data Program val = Program { getPrDefinitions :: [Definition val]
                           , getPrEntryPoint  :: Name
                           } deriving Show

instance Show a => Show (Term a) where
  show term = case term of
      (Val v)       -> '`' : show v
      (ValF _ args) -> "#func (" ++ showArguments args ++ ")"
      (ValP _ args) -> "#pred (" ++ showArguments args ++ ")"
      (Var v)       -> v
      (Con  c args) -> c ++ " (" ++ showArguments args ++ ")"
      (Fun f)       -> f
      (t1 :@ t2)    -> '(' : show t1 ++ ") (" ++ show t2 ++ ")"
      (x :-> t)     -> '\\' : x ++ " -> " ++ show t
      (Case t pm)   -> "case " ++ show t ++ " of" ++ foldl (\b a -> b ++ '\t' : show a ++ "\n") "\n" pm
      (Let x v t)   -> "let " ++ x ++ " = " ++ show v ++ " in " ++ show t
      where
          showArguments (arg : args) = foldl (\b a -> b ++ ", " ++ show a) (show arg) args
          showArguments [] = ""
