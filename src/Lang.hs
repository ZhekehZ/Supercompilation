module Lang where

--------------------------------------------------
--  Language DSL
--------------------------------------------------

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
              | Case { getCaseTerm             :: Term val bf bp
                     , getCases                :: PatternMatching val bf bp
                     }
              | Let  { getLetVarName           :: Name
                     , getLetValue             :: Term val bf bp
                     , getLetBody              :: Term val bf bp
                     }

type PatternMatching val bf bp = [PatternMatchingCase val bf bp]

data PatternMatchingCase val bf bp = Pattern :=> Term val bf bp


data Pattern = Pat { getPConstructor          :: Name
                   , getPConstructorVariables :: [Name]
                   }

data Definition val bf bp = Def Name (Term val bf bp)

data Program val bf bp = Program { getPrDefinitions :: [Definition val bf bp]
                                 , getPrEntryPoint  :: Name
                                 }

type BuiltinFunctionEval  val bf = bf -> [val] -> val
type BuiltinPredicateEval val bp = bp -> [val] -> Bool

infixr 0 :->
infixl 6 :@
infix 5 :=>

