module CodeGeneration where


import Lang
import Driving
import Utils
import Generalization
import Decomposition
import Control.Monad.Trans.State
import Control.Monad.Trans.Identity
import Data.Traversable

cogen :: Tree (Node val bf bp) -> [Name] -> Program val bf bp
cogen (Branch x ch) = undefined


data CogenState val bf bp = CGS { getDefinitions :: [Definition val bf bp]
                                , getFuncNames   :: [Name]
                                } deriving Show

cogen' :: (Show val, Show bf, Show bp) => [Name] -> Tree (Node val bf bp) -> StateT (CogenState val bf bp) Maybe (Term val bf bp)
cogen' st (Branch (Node term meta) ch) =
    case meta of
        Nothing -> case ch of
                    [ ] -> return term
                    [x] -> cogen' ([] : st) x
--         Just (MetaFun args) -> do
--             CGS defs (fun : funs) <- get
--             put $ CGS (Def fun args : defs) funs
--             ch (cogen' (fun : st))
--             return funcCallToArgsfun :@

        Just (MetaSplit term cases) -> do
            args <- for ch (cogen' ([] : st))
            return $ Case term [Pat c args :=> t | ((c, args), t) <- zip cases args]
        Just (MetaUp i subs) -> do
            return $ funcCallToApp (Left (st !! i), range subs)
        Just MetaLet -> do
            CGS defs (fun : funs) <- get
            args <- for ch (cogen' (fun : st))
            put $ CGS (Def fun (letToFun (last args) term) : defs) funs
            return $ funcCallToApp (Left fun, init args)
    where
        replaceT term (Let a v t) = Let a v (replaceT term t)
        replaceT term _ = term

        letToFun term (Let a v t) = a :-> letToFun term t
        letToFun term _ = term

compileTree tree = case runStateT (cogen' ["main"] tree) (CGS [] ["fun" ++ show i | i <- [1..]]) of
    Nothing -> error "compilation error"
    Just (main, CGS defs _) -> Program (Def "main" main : defs) "main"


{-
Just (fun1 1 0 case Gt(v, arg0) of {
        True() => case v' of {  },
        False() => fun1 Plus(v, 1) Plus(Mul(v', v'), v')
    },CGS {getDefinitions = [fun1 = let v = 1 in let v' = 0 in sum (squares (upto v arg0)) v'], getFuncNames = ["fun2","fun3"]})


-}