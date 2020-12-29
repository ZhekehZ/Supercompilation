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
    show = show . prettyPrintTerm False 0

instance (Show val, Show bf, Show bp) => Show (PatternMatchingCase val bf bp) where
    show = show . prettyPrintPMCase False

instance (Show val, Show bf, Show bp) => Show (Definition val bf bp) where
    show = show . prettyPrintDefinition False

instance (Show val, Show bf, Show bp) => Show (Program val bf bp) where
    show = show . prettyPrintProgram False


prettyPrintTerm :: (Show val, Show bf, Show bp) => Bool -> Int -> Term val bf bp -> Doc
prettyPrintTerm verbose p t = case t of
    Val v     -> vb "<val>" <> text (show v)
    Var v     -> vb "<var>" <> text v
    Fun v     -> vb "<fun>" <> text v
    ValF g as -> vb "<vfn>" <> pt (foldl (:@) (Fun $ toLowerCase $ show g) as)
    ValP g as -> vb "<vpd>" <> pt (foldl (:@) (Fun $ toLowerCase $ show g) as)
    Con  g as -> vb "<con>" <> pt (foldl (:@) (Fun g) as)
    a :@ b    -> printParen (p > 6) $ hsep [pc 6 a, pc 7 b]
    a :-> b   -> printParen (p > 0) $ text ('\\' : a ++ " -> ") <> pc 0 b
    Case e cs -> vcat [hsep [text "case", pc 0 e, text "of {"], vcat $ punctuate (text ";") (prettyPrintPMCase verbose <$> cs), text "}"]
    Let x e e' -> hsep [text $ "let " ++ x ++ " =", pc 0 e, text "in", pc 0 e']
    where printParen cond = if cond then parens else id
          pc = prettyPrintTerm verbose
          pt = pc p 
          toLowerCase = map (\s -> if s `elem` ['A'..'Z'] then chr (ord 'a' + ord s - ord 'A') else s)
          vb x = if verbose then text x else empty

prettyPrintPMCase :: (Show val, Show bf, Show bp) => Bool -> PatternMatchingCase val bf bp -> Doc
prettyPrintPMCase verbose (Pat c as :=> term) =  hcat [text "  ", hsep $ text <$> (c : as), text " => ", prettyPrintTerm verbose 0 term]

prettyPrintDefinition :: (Show val, Show bf, Show bp) => Bool -> Definition val bf bp -> Doc
prettyPrintDefinition verbose (Def name term) = let (as, t) = splitLam term in hsep $ text ("def " ++ name) : map text as 
                                                 ++ [text "=", prettyPrintTerm verbose 0 t]
       where splitLam (x :-> t) = case splitLam t of (xs, t) -> (x:xs, t)    
             splitLam t = ([], t)

prettyPrintProgram :: (Show val, Show bf, Show bp) => Bool -> Program val bf bp -> Doc
prettyPrintProgram verbose (Program defs entry) = vcat $ prettyPrintTerm verbose 0 main : 
              if null odefs then [] else text "where" : map (prettyPrintDefinition verbose) odefs
       where Def _ main = fromJust $ find (\(Def n _) -> n == entry) defs
             odefs = filter (\(Def n _) -> n /= entry) defs
