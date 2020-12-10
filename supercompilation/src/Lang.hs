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
                     } deriving Show

infixr 4 :->
infixl 6 :@

type PatternMatching val bf bp = [PatternMatchingCase val bf bp]

data PatternMatchingCase val bf bp = Pattern :=> Term val bf bp deriving Show
infix 5 :=>

data Pattern = Pat { getPConstructor          :: Name
                   , getPConstructorVariables :: [Name]
                   } deriving Show

data Definition val bf bp = Def Name (Term val bf bp) deriving Show

data Program val bf bp = Program { getPrDefinitions :: [Definition val bf bp]
                                 , getPrEntryPoint  :: Name
                                 } deriving Show

type BuiltinFunctionEval val bf = bf -> [val] -> val
type BuiltinPredicateEval val bp = bp -> [val] -> Bool
