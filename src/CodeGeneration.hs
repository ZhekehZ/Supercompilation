module CodeGeneration where


import Lang
import Driving
import Utils
import Eval
import Generalization
import Decomposition
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Identity
import Data.Traversable
import Data.List
import Data.Maybe

cogen :: Tree (Node val bf bp) -> [Name] -> Program val bf bp
cogen (Branch x ch) = undefined


data CogenState val bf bp = CGS { getDefinitions :: [Definition val bf bp]
                                , getFuncNames   :: [Name]
                                } deriving Show

cogen' :: [(Name, [Name])] -> Tree (Node val bf bp) -> StateT (CogenState val bf bp) Maybe (Term val bf bp)
cogen' st (Branch (Node term meta) ch) =
    case meta of
        Nothing -> case ch of
                    [ ] -> return term
                    [x] -> cogen' (("",[]) : st) x

        Just (MetaFun args) -> do
                    CGS defs (fun : funs) <- get
                    [body] <- for ch (cogen' ((fun, args) : st))
                    put $ CGS (Def fun (makeFun args body) : defs) funs
                    return $ funcCallToApp (Left fun, (Var <$> args))

        Just (MetaSplit term cases) -> do
            args <- for ch (cogen' (("",[]) : st))
            return $ Case term [Pat c args :=> t | ((c, args), t) <- zip cases args]

        Just (MetaUp i subs) -> do
            let (fun, argNames) = st !! (i - 1)
            let args = [fromMaybe (Var an) ((\(x := t) -> t) <$> find (\(x := t) -> x == an) subs) | an <- argNames]
            return $ funcCallToApp (Left fun, args)

        Just MetaLet -> do
            args <- for ch (cogen' (("", []) : st))
            return $ replaceT (last args) term
    where
        makeFun []     body = body
        makeFun (x:xs) body = x :-> makeFun xs body

        replaceT term (Let a v t) = Let a v (replaceT term t)
        replaceT term _ = term

        letToFun term (Let a v t) = a :-> letToFun term t
        letToFun term _ = term

compileTree tree = case runStateT (cogen' [] tree) (CGS [] ["fun" ++ show i | i <- [1..]]) of
    Nothing -> error "compilation error"
    Just (main, CGS defs _) -> Program (Def "main" (forceLets main) : defs) "main"


forceLets expression = case expression of
  (Let x e t)  -> subst e x (forceLets t)
  x -> x