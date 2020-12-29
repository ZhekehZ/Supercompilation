module Parser where

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Expr
import Control.Monad
import Data.List
import Data.Maybe

import Lang
import Utils
import IntProgram

data IRExpr = IVar Name
            | IVal Int 
            | IBiFun BuiltinF IRExpr IRExpr 
            | IBiPre BuiltinP IRExpr IRExpr 
            | ICase IRExpr [(Name, [Name], IRExpr)]
            | IDef Name [Name] IRExpr 
            | ICall Name [IRExpr]
            deriving Show

parseProgram :: String -> PROGRAM
parseProgram text = 
    case runParser (do { t_whiteSpace
                       ; main <- convertIRtoTerm <$> expr
                       ; defs <- optionMaybe (t_reserved "where" >> many (convertIRtoDef <$> definition))
                       ; return $ Program (Def "main" main : fromMaybe [] defs) "main"
                       }
                   ) () "" text of
        Right p -> p
        Left er -> error $ show er
    where

    lanDef :: LanguageDef st
    lanDef = emptyDef{ commentStart    = "{-"
                     , commentEnd      = "-}"
                     , identStart      = letter
                     , commentLine    = "--"
                     , nestedComments  = True
                     , identLetter     = alphaNum <|> oneOf "_'"
                     , opStart         = opLetter lanDef
                     , opLetter        = oneOf $ map head (reservedOpNames lanDef)
                     , reservedOpNames = ["+", "-", "*", "/", "%", "=", "==", ">", "<", "=>"]
                     , reservedNames   = ["case", "of", "def", "where"]
                     , caseSensitive   = True
                     }

    token@TokenParser{ parens     = t_parens
                     , identifier = t_identifier
                     , reservedOp = t_reservedOp
                     , reserved   = t_reserved
                     , semiSep1   = t_semiSep1
                     , whiteSpace = t_whiteSpace 
                     } = makeTokenParser lanDef

    expr = buildExpressionParser table stmt <?> "expression"

    table = [ [Infix (t_reservedOp "*"   >> return (IBiFun Mul  )) AssocLeft,
                Infix (t_reservedOp "/"  >> return (IBiFun Div  )) AssocLeft,
                Infix (t_reservedOp "%"  >> return (IBiFun Mod  )) AssocLeft]
            , [Infix (t_reservedOp "+"   >> return (IBiFun Plus )) AssocLeft, 
                Infix (t_reservedOp "-"  >> return (IBiFun Minus)) AssocLeft]
            , [Infix (t_reservedOp ">"   >> return (IBiPre Gt   )) AssocLeft, 
                Infix (t_reservedOp "<"  >> return (IBiPre Lt   )) AssocLeft, 
                Infix (t_reservedOp "==" >> return (IBiPre Eql  )) AssocLeft]
            ]

    stmt = (ICall <$> t_identifier <*> many (term <|> fmap IVar t_identifier)) <|> term

    term =  t_parens expr
        <|> fmap (IVal . fromIntegral) (natural token)
        <|> do { t_reserved "case"
               ; t <- expr
               ; t_reserved "of"
               ; cases <- braces token (t_semiSep1 pmCase)
               ; return $ ICase t cases
               }

    pmCase = do { con <- t_identifier
                ; args <- many t_identifier
                ; t_reserved "=>"
                ; t <- expr
                ; return (con, args, t)
                }

    definition = do { t_reserved "def"
                    ; name <- t_identifier
                    ; args <- many1 t_identifier
                    ; t_reservedOp "="
                    ; IDef name args <$> expr
                    } 

    convertIRtoTerm :: IRExpr -> TERM
    convertIRtoTerm ir = case ir of 
        IVal i        -> Val i
        IVar v        -> if isCon v then Con v [] else Var v
        IBiFun bf a b -> ValF bf $ convertIRtoTerm <$> [a, b] 
        IBiPre bp a b -> ValP bp $ convertIRtoTerm <$> [a, b] 
        ICase t cases -> Case (convertIRtoTerm t) $ (\(con, args, t) -> Pat con args :=> convertIRtoTerm t) <$> cases
        ICall v args  -> if isCon v then Con v (convertIRtoTerm <$> args)
                                    else case args of 
                                        [] -> Var v
                                        _  -> case builtinParser v of 
                                                Nothing       -> foldl (\t arg -> t :@ convertIRtoTerm arg) (Fun v) args
                                                Just (Left  f) -> ValF f (convertIRtoTerm <$> args)
                                                Just (Right p) -> ValP p (convertIRtoTerm <$> args)
        where isCon v = head v `elem` ['A'..'Z']

    convertIRtoDef :: IRExpr -> DEFINITION
    convertIRtoDef (IDef fun args body) = Def fun $ foldr (:->) (convertIRtoTerm body) args 
    

builtinParser :: String -> Maybe (Either BuiltinF BuiltinP)
builtinParser "plus"  = Just $ Left Plus
builtinParser "minus" = Just $ Left Minus
builtinParser "mul"   = Just $ Left Mul
builtinParser "div"   = Just $ Left Div
builtinParser "mod"   = Just $ Left Mod
builtinParser "gt"    = Just $ Right Gt
builtinParser "eql"   = Just $ Right Eql
builtinParser "lt"    = Just $ Right Lt
builtinParser  _      = Nothing
