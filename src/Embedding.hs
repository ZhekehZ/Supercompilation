module Embedding where

import Lang
import Utils
import Eval
import TermEq

-- Homeomorphic embedding
(<|) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
a <| b = (a <|. b) || (a <|.. b) || (a <|... b)

-- Variable
(<|.) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
Var _ <|. Var _ = True
Val x <|. Val y = x == y
_     <|. _     = False

-- Coupling
(<|..) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
a <|.. b | Just (h ,  args) <- appAsFuncCall a
         , Just (h', args') <- appAsFuncCall b = h == h' && all (uncurry (<|)) (zip args args') 
a <|..b = False


-- Diving
(<|...) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
a <|... b | Just (_,  args) <- appAsFuncCall b = any (a <|) args 
a <|...b = False
