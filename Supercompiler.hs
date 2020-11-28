module Supercompiler where

import Control.Applicative

type Name = String
                                                                   -- Term example
data Expr val = Val  { getValue :: val }                           -- 1
              | ValF { builtinFunction :: [val] -> val             -- +
                     , arguments :: [Expr val]
                     }
              | ValP { builtinPredicate :: [val] -> Bool           -- ==
                     , arguments :: [Expr val]
                     }
              | Var { varName :: Name }                            -- x
              | Con { constructor :: Name                          -- C (x1, ..., xn)
                    , constructorArguments :: [Expr val]
                    }
              | Fun { function :: Name }                           -- foo
              | Name :-> Expr val                                  -- \x -> e
              | Expr val :@ Expr val                               -- e1 e2
              | Case { caseVariable :: Expr val                    -- case e of {p1 -> e1, ..., pn -> en}
                     , cases :: [PMCase val]
                     }
              | Let { letVarName :: Name                           -- let x = e1 in e2
                    , letValue :: Expr val
                    , letBody :: Expr val
                    }
infix 5 :=>
infixr 4 :->

instance Show val => Show (Expr val) where
  show (Val v) = "Val " ++ show v
  show (ValF v args) = "ValF ?? " ++ show args
  show (ValP v args) = "ValP ?? " ++ show args
  show (Var v) = "Var " ++ v
  show (Con c args) = "Con " ++ c ++ " " ++ show args
  show (Fun f) = "Fun " ++ f
  show (a:->b) = "\\" ++ a ++ " -> " ++ show b
  show (a:@b) = "(" ++ show a ++ ")(" ++ show b ++ ")"
  show (Case e xs) = "Case " ++ show e ++ show xs
  show (Let x e1 e2) = "Let " ++ x ++ " " ++ show e1 ++ " in " ++ show e2

data PMCase val = Pattern :=> Expr val
  deriving Show

data Pattern = Pat { pConstructor :: Name
                   , pConstructorVariables :: [Name]
                   }
  deriving Show

data Def val = Def Name (Expr val)
  deriving Show

data Program val = Program {  programDefinitions :: [Def val]
                           ,  entryPoint         :: Name
                           }
  deriving Show


testProgram :: Program Int
testProgram = Program { programDefinitions =
                          [ Def "sum" $ "xs" :-> "a" :->
                                Case (Var "xs")
                                [ Pat "Nil"  []          :=> Var "a"
                                , Pat "Cons" ["x", "xs"] :=> Fun "sum" :@ Var "xs" :@ (plus [Var "x", Val 0])
                                ]

                          , Def "squares" $ "xs" :->
                                Case (Var "xs")
                                [ Pat "Nil"  []          :=> Con "Nil" []
                                , Pat "Cons" ["x", "xs"] :=> Con "Cons" [mul [Var "x", Var "x"], Fun "squares" :@ Var "xs"]
                                ]

                          , Def "upto" $ "m" :-> "n" :->
                                Case (gt [Var "m", Var "n"])
                                [ Pat "True"  [] :=> Con "Nil" []
                                , Pat "True"  [] :=> Con "Cons" [Var "m", Fun "upto" :@ (plus [Var "m", Val 1]) :@ Var "n"]
                                ]

                          , Def "main" $ Fun "sum" :@ (Fun "squares" :@ (Fun "upto" :@ Val 1 :@ Var "arg0")) :@ Val 1
                          ]
                      , entryPoint = "main"
                      }
      where
          [plus, mul] = map (\f -> ValF $ \[x, y] -> f x y) [(+), (*)]
          [gt]        = map (\f -> ValP $ \[x, y] -> f x y) [(>)]

type Context val = [(Name, Expr val)]

evalProgram :: Program val -> Context val -> Maybe (Expr val)
evalProgram Program {programDefinitions=def, entryPoint=entry} args = evalExpr args def (lookupFun def entry)

lookupFun :: [Def val] -> Name -> Expr val
lookupFun (Def name' expr : defs) name | name == name' = expr
                                       | otherwise     = lookupFun defs name
lookupFun [] name = error $ "Invalid function name: " ++ name

evalExpr' c d e = maybe e (evalExpr' c d) $ evalExpr c d e

subst :: Expr val -> Name -> Expr val -> Expr val
subst e n u = case u of
    Val v       -> Val v
    ValF f args -> ValF f (subst' <$> args)
    ValP p args -> ValP p (subst' <$> args)
    Var x       -> if x == n then e else Var x
    Con c args  -> Con c (subst' <$> args)
    Fun f       -> Fun f
    x :-> t     -> undefined
    e1 :@ e2    -> subst' e1 :@ subst' e2
    Case e pmc  -> undefined
    Let x e t   -> undefined
    where
        subst' = subst e n

evalExpr :: Context val -> [Def val] -> Expr val -> Maybe (Expr val)
evalExpr context defines expression = case expression of
  ValF f args -> (ValF f <$> evalArguments args) <|> (Just $ Val $ f (getValue <$> args))
  ValP p args -> (ValP p <$> evalArguments args) <|> (Just $ Con (show $ p (getValue <$> args)) [])
  Var v       -> lookup v context
  Fun f       -> Just $ lookupFun defines f
  ((x:->e):@t)-> Just $ subst t x e
  (Let x e t) -> (\e -> subst e x t) <$> eval e
  (e1:@e2)    -> (:@e2) <$> eval e1
  _ -> Nothing

  where
      eval = evalExpr context defines
      evalArguments (x:xs) = ((:xs) <$> eval x) <|>  ((x:) <$> evalArguments xs)
      evalArguments []     = Nothing

--main :: IO ()
--main = putStrLn (show (programDefinitions testProgram))