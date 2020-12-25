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
                        return $ funcCallToApp (Left fun, (Var <$> args))

        MetaSplit term cases -> do
            term : args <- for ch (cogen (("",[]) : st))
            return $ Case term [Pat c args :=> t | ((c, args), t) <- zip cases args]

        MetaUp i subs -> do
            let (fun, argNames) = st !! (i - 1)
            return $ funcCallToApp (Left fun, [fromMaybe (Var an) (findByDom subs an) | an <- argNames])

        MetaLet -> do
            return $ term
    where
        makeFun []     body = body
        makeFun (x:xs) body = x :-> makeFun xs body

        replaceT term (Let a v t) = Let a v (replaceT term t)
        replaceT term _ = term

        letToFun term (Let a v t) = a :-> letToFun term t
        letToFun term _ = term


compileTree ::  (Show val, Show bf, Show bp) => Tree (Node val bf bp) -> Program val bf bp
compileTree tree = case runStateT (cogen [] tree) (CGS [] ["fun" ++ show i | i <- [1..]]) of
    Nothing -> error "compilation error"
    Just (main, CGS defs q) -> Program (Def "main" (forceLets main) : defs) "main"
    where
        forceLets expression = case expression of
            (Let x e t)  -> subst e x (forceLets t)
            x -> x


-- fun1 = \s -> case case s of { 
--                     Nil => False, 
--                     Cons(v1, v3) => case cmp v1 A of { 
--                             True => m1 Cons(B, Nil) v3 Cons(A, Cons(B, Nil)) s, 
--                             False => next s Cons(A, Cons(B, Nil)) 
--                     } 
--             } of { 
--                 Nil => False, 
--                 Cons(v1, v3) => case case case v1 of { 
--                     A => case A of { 
--                         A => True, 
--                         B => False, 
--                         C => False 
--                     }, 
--                     B => case A of { 
--                         A => False, 
--                         B => True, 
--                         C => False 
--                     }, 
--                     C => case A of { 
--                         A => False, 
--                         B => False, 
--                         C => True 
--                     } 
--                 } of { 
--                     True => m1 Cons(B, Nil) v3 Cons(A, Cons(B, Nil)) Cons(v1, v3), False => next Cons(v1, v3) Cons(A, Cons(B, Nil)) } of { A => fun2 v3, B => fun1 v3, C => fun1 v3 } }
-- fun2 = \v3 -> case case v3 of { Nil => False, Cons(v1, v4) => case cmp v1 B of { True => m1 Nil v4 Cons(A, Cons(B, Nil)) Cons(A, v3), False => next Cons(A, v3) Cons(A, Cons(B, Nil)) } } of { Nil => False, Cons(v1, v4) => case case case v1 of { A => case B of { A => True, B => False, C => False }, B => case B of { A => False, B => True, C => False }, C => case B of { A => False, B => False, C => True } } of { True => m1 Nil v4 Cons(A, Cons(B, Nil)) Cons(A, Cons(v1, v4)), False => next Cons(A, Cons(v1, v4)) Cons(A, Cons(B, Nil)) } of { A => fun2 v4, B => True, C => fun1 v4 } }