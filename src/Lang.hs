module Lang where

import Prelude hiding ((<>))
import Text.PrettyPrint
import Data.List
import Data.Char
import Data.Maybe

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
    ValF g as -> pt $ foldl (:@) (Fun $ toLowerCase $ show g) as
    ValP g as -> pt $ foldl (:@) (Fun $ toLowerCase $ show g) as
    Con  g as -> text g <> if null as then empty else printArgs as 
    a :@ b    -> printParen (p > 6) $ hsep [prettyPrintTerm 6 a, prettyPrintTerm 7 b]
    a :-> b   -> printParen (p > 0) $ text ('\\' : a ++ " -> ") <> prettyPrintTerm 0 b
    Case e cs -> vcat [hsep [text "case", prettyPrintTerm 0 e, text "of {"], vcat (prettyPrintPMCase <$> cs), text "}"]
    Let x e e' -> hsep [text $ "let " ++ x ++ " =", prettyPrintTerm 0 e, text "in", prettyPrintTerm 0 e']
    where printArgs as = parens (hsep $ punctuate (text ",") (prettyPrintTerm 0 <$> as))
          printParen cond = if cond then parens else id
          pt = prettyPrintTerm p 
          toLowerCase = map (\s -> if s `elem` ['A'..'Z'] then chr (ord 'a' + ord s - ord 'A') else s)

prettyPrintPMCase :: (Show val, Show bf, Show bp) => PatternMatchingCase val bf bp -> Doc
prettyPrintPMCase (Pat c as :=> term) = hcat [text "  ", hsep $ text <$> (c : as), text " => ", prettyPrintTerm 0 term, text ";"]

prettyPrintDefinition :: (Show val, Show bf, Show bp) => Definition val bf bp -> Doc
prettyPrintDefinition (Def name term) = let (as, t) = splitLam term in hsep $ text ("def " ++ name) : map text as 
                                                 ++ [text "=", prettyPrintTerm 0 t]
       where splitLam (x :-> t) = case splitLam t of (xs, t) -> (x:xs, t)    
             splitLam t = ([], t)

prettyPrintProgram :: (Show val, Show bf, Show bp) => Program val bf bp -> Doc
prettyPrintProgram (Program defs entry) = vcat $ prettyPrintTerm 0 main : text "where" : map prettyPrintDefinition odefs
       where Def _ main = fromJust $ find (\(Def n _) -> n == entry) defs
             odefs = filter (\(Def n _) -> n /= entry) defs
