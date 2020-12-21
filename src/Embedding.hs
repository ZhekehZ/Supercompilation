module Embedding where

import Lang
import Utils

-- Homeomorphic embedding
(<|) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
a <| b = (a <|. b) || (a <|.. b) || (a <|... b)

-- Variable
(<|.) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
Var _ <|. Var _ = True
x     <|. y     = x == y

-- Coupling
(<|..) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
f_e1_en <|.. f_e1_en' = case (appAsFuncCall f_e1_en, appAsFuncCall f_e1_en') of
  (Just (f, as), Just (f', as')) | f == f' -> all (uncurry (<|)) (zip as as')
  _                                        -> False

-- Diving
(<|...) :: (Eq val, Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
e <|... h_e1_en = case appAsFuncCall h_e1_en of
  Just (_, as) -> any (e <|) as
  _            -> False
