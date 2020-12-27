module Parser where

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Language
import Control.Monad

import Lang
import Utils
import IntProgram

------------------------------------------------------
-- SYNTAX CONFIG

synt = ["case", "of", "def", "where"]
builtins = ["plus", "mul", "gt"]

parseBuiltin :: String -> Either BuiltinF BuiltinP
parseBuiltin "plus" = Left Plus
parseBuiltin "mul" = Left Mul
parseBuiltin "gt" = Right Gt

------------------------------------------------------
-- HELPERS

tok :: String -> Parsec String () ()
tok = void . trim . string

parens :: String -> Parsec String () a -> Parsec String () a
parens [op, cls] p = do
    void $ trim $ char op
    res <- trim p
    void $ trim $ char cls
    return res

trim :: Parsec String () a -> Parsec String () a
trim p = do
    res <- p
    many blank
    return res

blank :: Parsec String () ()
blank = void $ oneOf [' ', '\t'] 

lident :: Parsec String () String
lident = trim $ (:) <$> oneOf ['a'..'z'] <*> many alphaNum

uident :: Parsec String () String
uident = trim $ (:) <$> oneOf ['A'..'Z'] <*> many alphaNum

nat :: Parsec String () Int
nat = read <$> trim (many1 $ oneOf ['0'..'9'])

------------------------
-- PARSER

var :: Parsec String () TERM
var = do 
    name <- lookAhead lident
    when (name `elem` synt) $ parserFail $ "invalid variable name: " ++ name
    Var <$> lident

val :: Parsec String () TERM
val = Val <$> nat 

func :: Parsec String () TERM
func = do
    f <- lident
    as <- many arg    
    let h = if f `notElem` builtins then Left f else Right $ parseBuiltin f
    return $ if null as then Var f else funcCallToApp (h, as)

-- Parse function call argument
arg :: Parsec String () TERM
arg =  var                       
   <|> val                      
   <|> flip Con [] <$> uident   
   <|> parens "()" term               
   <|> ccase

term :: Parsec String () TERM
term =  parens "()" term 
    <|> ccase
    <|> func 
    <|> var 
    <|> val
    <|> cons

cons :: Parsec String () TERM
cons = Con <$> uident <*> many arg

ccase :: Parsec String () TERM
ccase = do
    tok "case"
    t <- term
    tok "of"
    pmcases <- parens "{}" $ many1 pmCase
    return $ Case t pmcases


pmCase :: Parsec String () (PatternMatchingCase Int BuiltinF BuiltinP)
pmCase = do
    con <- uident
    args <- fmap getVarName <$> many var
    tok "=>"
    t <- term
    optional $ tok ";"
    return $ Pat con args :=> t

def :: Parsec String () (Definition Int BuiltinF BuiltinP)
def = do
    tok "def"
    name <- lident
    args <- fmap getVarName <$> many var
    tok "="
    Def name . argsToLam args <$> term
    where argsToLam = flip (foldr (:->))

prog :: Parsec String () PROGRAM
prog = do
    many blank
    main <- term
    tok "where"
    defs <- many def
    return (Program (Def "main" main : defs) "main")


parseProg :: String -> PROGRAM
parseProg = either (error "Parse error") id . runParser prog () "" . concatMap (' ':) . lines
