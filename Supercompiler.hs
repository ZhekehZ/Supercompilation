module Supercompiler where

type Name = String
                                                                   -- Term example
data Expr val = Val val                                            -- 1
              | ValF { arity :: Int                                -- +
                     , builtinFunction :: [val] -> val
                     }
              | ValP { arity :: Int                                -- ==
                     , builtinPredicate :: [val] -> Bool
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

data PMCase val = Pattern :=> Expr val

data Pattern = Pat { pConstructor :: Name
                   , pConstructorVariables :: [Name]
                   }

data Def val = Def Name (Expr val)

data Program val = Program {  programDefinitions :: [Def val]
                           ,  entryPoint         :: Name
                           }


testProgram :: Program Int
testProgram = Program { programDefinitions =
                          [ Def "sum" $ "xs" :-> "a" :->
                                Case (Var "xs")
                                [ Pat "Nil"  []          :=> Var "a"
                                , Pat "Cons" ["x", "xs"] :=> Fun "sum" :@ Var "xs" :@ (plus :@ Var "x" :@ Val 0)
                                ]

                          , Def "squares" $ "xs" :->
                                Case (Var "xs")
                                [ Pat "Nil"  []          :=> Con "Nil" []
                                , Pat "Cons" ["x", "xs"] :=> Con "Cons" [mul :@ Var "x" :@ Var "x", Fun "squares" :@ Var "xs"]
                                ]

                          , Def "upto" $ "m" :-> "n" :->
                                Case (gt :@ Var "m" :@ Var "n")
                                [ Pat "True"  [] :=> Con "Nil" []
                                , Pat "True"  [] :=> Con "Cons" [Var "m", Fun "upto" :@ (plus :@ Var "m" :@ Val 1) :@ Var "n"]
                                ]

                          , Def "main" $ Fun "sum" :@ (Fun "squares" :@ (Fun "upto" :@ Val 1 :@ Var "arg0")) :@ Val 1
                          ]
                      , entryPoint = "main"
                      }
      where
          [plus, mul] = map (\f -> ValF 2 $ \[x, y] -> f y x) [(+), (*)]
          [gt]        = map (\f -> ValP 2 $ \[x, y] -> f y x) [(>)]


type Context val = [(Name, Expr val)]

evalProgram :: Program val -> Context val -> Expr val
evalProgram Program {programDefinitions=def, entryPoint=entry} args = evalExpr args def (lookupFun def entry)

lookupFun :: [Def val] -> Name -> Expr val
lookupFun (Def name' expr : defs) name | name == name' = expr
                                       | otherwise     = lookupFun defs name
lookupFun [] name = error $ "Invalid function name: " ++ name

evalExpr :: Context val -> [Def val] -> Expr val -> Expr val
evalExpr c d (Val v) = Val v
evalExpr c d (ValF a builtinF) = Val $ builtinF (map (\x -> case x of (_, Val v) -> v) $ take a c)
evalExpr c d (ValP a builtinP) = Con (if builtinP (map (\x -> case x of (_, Val v) -> v) $ take a c) then "True" else "False") []
evalExpr c d (Var name) = case lookup name c of
                            Just e -> e
                            Nothing -> error $ "Invalid variable name (" ++ name ++ ")"
evalExpr c d (Con cname args) = undefined
evalExpr c d (Fun f) = undefined
evalExpr c d (n :-> e)  = undefined
evalExpr c d (e1 :@ e2) = undefined
evalExpr c d (Case e ps) = undefined
evalExpr c d (Let x v e) = undefined

--main :: IO ()
--main = putStrLn (show (programDefinitions testProgram))