module Lang where

import Prelude hiding ((<>))
import Text.PrettyPrint
import Data.List

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

instance (Show val, Show bf, Show bp) => Show (Term val bf bp) where
    show = show . prettyPrintTerm 0

instance (Show val, Show bf, Show bp) => Show (PatternMatchingCase val bf bp) where
    show = show . prettyPrintPMCase

instance (Show val, Show bf, Show bp) => Show (Definition val bf bp) where
    show = show . prettyPrintDefinition

instance (Show val, Show bf, Show bp) => Show (Program val bf bp) where
    show = show . prettyPrintProgram


prettyPrintTerm :: (Show val, Show bf, Show bp) => Int -> Term val bf bp -> Doc
prettyPrintTerm p t = case t of
    Val v     -> text (show v)
    Var v     -> text v
    Fun v     -> text v
    ValF f as -> text (show f) <> printArgs as 
    ValP f as -> text (show f) <> printArgs as 
    Con  f as -> text f <> if null as then empty else printArgs as 
    a :@ b    -> printParen (p > 6) $ hcat [prettyPrintTerm 6 a, char ' ', prettyPrintTerm 7 b]
    a :-> b   -> printParen (p > 0) $ text ('\\' : a ++ " -> ") <> prettyPrintTerm 0 b
    Case e cs -> vcat [hcat [text "case ", prettyPrintTerm 0 e, text " of {"], vcat (prettyPrintPMCase <$> cs), text "}"]
    Let x e e' -> hcat [text $ "let " ++ x ++ " = ", prettyPrintTerm 0 e, text " in ", prettyPrintTerm 0 e']
    where printArgs as = parens (sep $ punctuate (text ",") (prettyPrintTerm 0 <$> as))
          printParen cond = if cond then parens else id

prettyPrintPMCase :: (Show val, Show bf, Show bp) => PatternMatchingCase val bf bp -> Doc
prettyPrintPMCase (Pat c as :=> term) = hcat [text ("  " ++ c) <> if null as then empty else parens $ sep $ punctuate (text ",") (text <$> as)
                                             , text " => ", prettyPrintTerm 0 term]

prettyPrintDefinition :: (Show val, Show bf, Show bp) => Definition val bf bp -> Doc
prettyPrintDefinition (Def name term) = hcat [text name, text " = ", prettyPrintTerm 0 term] 

prettyPrintProgram :: (Show val, Show bf, Show bp) => Program val bf bp -> Doc
prettyPrintProgram (Program defs entry) = vcat $ text ("Program (entry = " ++ entry ++ "):") : fmap prettyPrintDefinition defs
