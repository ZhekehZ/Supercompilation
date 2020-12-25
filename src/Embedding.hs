module Embedding where

import Lang
import Utils
import Eval
import Instances

-- Homeomorphic embedding
(<|) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
a <| b = (a <|. b) || (a <|.. b) || (a <|... b)

-- Variable
(<|.) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
Var _ <|. Var _ = True
_     <|. _     = False

-- Coupling
(<|..) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
(Fun f   ) <|.. (Fun g   ) = f == g
(f :@ fs ) <|.. (g :@ gs ) = (fs <| gs) && (f <|.. g)
(Con c a1) <|.. (Con r a2) = c == r && (all (uncurry (<|)) $ zip a1 a2)
(_       ) <|.. (_       ) = False


-- Diving
(<|...) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
e       <|... (f :@ a ) = (e <| a) || (e <|... f)
e@Con{} <|... (Con _ a) = any (e <|) a
_       <|... (_      ) = False
