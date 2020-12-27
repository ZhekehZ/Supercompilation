module CodeGeneration where

import Lang
import Driving
import Utils
import Eval
import Generalization
import Decomposition
import ProcessTree
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Identity
import Data.Traversable
import Data.List
import Data.Maybe
import Data.Foldable


data CogenState val bf bp = CGS { getDefinitions :: [Definition val bf bp]
                                , getFuncNames   :: [Name]
                                } deriving Show


cogen :: (Show val, Show bf, Show bp) => [(Name, [Name])] -> Tree (Node val bf bp) -> StateT (CogenState val bf bp) Maybe (Term val bf bp)
cogen st (Branch (Node term meta) ch) =
    case meta of
        Regular -> case ch of
                    [ ] -> return term
                    [x] -> cogen (("",[]) : st) x

        MetaFun args -> do
                        CGS defs (fun : funs) <- get
                        modify (\(CGS defs (_:x)) -> CGS defs x)
                        [body] <- forM ch (cogen ((fun, args) : st))
                        modify (\(CGS defs x) -> CGS (Def fun (makeFun args body) : defs) x)
                        return $ funcCallToApp (Left fun, Var <$> args)

        MetaSplit term cases -> do
            term : args <- for ch (cogen (("",[]) : st))
            return $ Case term [Pat c args :=> t | ((c, args), t) <- zip cases args]

        MetaUp i subs -> do
            let (fun, argNames) = st !! (i - 1)
            return $ funcCallToApp (Left fun, [fromMaybe (Var an) (findByDom subs an) | an <- argNames])

        MetaLet -> do
            args <- for ch (cogen (("", []) : st))
            return $ substLam args term
    where
        makeFun = flip (foldr (:->))

        substLam [a] t = a
        substLam (a:as) (Let x _ t) = subst a x (substLam as t)


compileTree ::  (Show val, Show bf, Show bp) => Tree (Node val bf bp) -> Program val bf bp
compileTree tree = case runStateT (cogen [] tree) (CGS [] ["fun" ++ show i | i <- [1..]]) of
    Nothing -> error "compilation error"
    Just (main, CGS defs q) -> Program (Def "main" (forceLets main) : defs) "main"
    where
        forceLets expression = case expression of
            (Let x e t)  -> subst e x (forceLets t)
            x -> x
