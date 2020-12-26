module Parser where

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Language
import Control.Monad

import Lang
import Utils
import IntProgram

synt = ["case", "of", "def", "where"]
builtins = ["plus", "mul", "gt"]

parseBuiltin :: String -> Either BuiltinF BuiltinP
parseBuiltin "plus" = Left Plus
parseBuiltin "mul" = Left Mul
parseBuiltin "gt" = Right Gt


parens :: Parsec String () a -> Parsec String () a
parens p = do
    void $ andSp $ char '('
    res <- andSp p
    void $ andSp $ char ')'
    return res

andSp :: Parsec String () a -> Parsec String () a
andSp p = do
    res <- p
    many blank
    return res

blank :: Parsec String () ()
blank = void $ char ' ' <|> char '\t'

lident :: Parsec String () String
lident = andSp $ (:) <$> oneOf ['a'..'z'] <*> many alphaNum

uident :: Parsec String () String
uident = andSp $ (:) <$> oneOf ['A'..'Z'] <*> many alphaNum

------------------------

var :: Parsec String () TERM
var = do 
    name <- lookAhead lident
    when (name `elem` synt) $ parserFail $ "invalid name: " ++ name
    Var <$> lident

val :: Parsec String () TERM
val = do
    iS <- andSp $ many1 $ oneOf ['0'..'9']
    let i = read iS :: Int 
    return (Val i) 

func :: Parsec String () TERM
func = do
    f <- lident
    as <- many arg    
    let h = if f `notElem` builtins then Left f else Right $ parseBuiltin f
    return $ if null as then Var f else funcCallToApp (h, as)

arg :: Parsec String () TERM
arg =  var 
   <|> val
   <|> parens term
   <|> flip Con [] <$> uident 
   <|> ccase

term :: Parsec String () TERM
term =  parens term 
    <|> ccase
    <|> func 
    <|> var 
    <|> val
    <|> cons

cons :: Parsec String () TERM
cons = Con <$> uident <*> many arg

ccase :: Parsec String () TERM
ccase = do
    andSp $ void (string "case")
    t <- term
    andSp $ void (string "of")
    andSp $ void (char '{')
    pmcases <- many1 pmCase
    andSp $ void (char '}')
    return $ Case t pmcases


pmCase :: Parsec String () (PatternMatchingCase Int BuiltinF BuiltinP)
pmCase = do
    con <- uident
    argsV <- many var
    let args = (\(Var v) -> v) <$> argsV
    andSp $ void (string "=>")
    t <- term
    optional $ andSp $ char ';'
    return $ Pat con args :=> t

def :: Parsec String () (Definition Int BuiltinF BuiltinP)
def = do
    andSp $ void (string "def")
    name <- lident
    argsV <- many1 var
    let args = (\(Var v) -> v) <$> argsV
    andSp $ void (char '=')
    Def name . argsToLam args <$> term
    where argsToLam = flip (foldr (:->))

prog :: Parsec String () PROGRAM
prog = do
    many blank
    main <- term
    andSp $ void (string "where")
    defs <- many def
    return (Program (Def "main" main : defs) "main")


parseFile :: String -> IO PROGRAM
parseFile fileName = do
    text <- readFile fileName
    let oneline = map (\x -> if x == '\n' then ' ' else x) text
    let Right program = runParser prog () "" oneline
    return program