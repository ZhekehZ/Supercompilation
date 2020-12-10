module SubTerm where

import Lang

(<|) :: (Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
a <| b = (a <|. b) || (a <|.. b) || (a <|... b)

-- Variable
(<|.) :: Term val bf bp -> Term val bf bp -> Bool
Var _ <|. Var _ = True
_     <|. _     = False

-- Coupling
(<|..) :: (Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
f_e1_en <|.. f_e1_en' = case (appAsFuncCall f_e1_en, appAsFuncCall f_e1_en') of
    (Just (f, as), Just (f', as')) -> (f == f') && all (uncurry (<|)) (zip as as')
    _                              -> False

-- Diving
(<|...) :: (Eq bf, Eq bp) => Term val bf bp -> Term val bf bp -> Bool
e <|... h_e1_en = case appAsFuncCall h_e1_en of
  Just (_, as) -> any (e <|) as
  _            -> False


type Either3 a b c = Either a (Either b c)

appAsFuncCall :: Term val bf bp -> Maybe (Either3 Name bf bp, [Term val bf bp])
appAsFuncCall (a :@ b)      = (\(name, args) -> (name, args ++ [b])) <$> appAsFuncCall a
appAsFuncCall (Fun f)       = Just (Left f, [])
appAsFuncCall (ValF f args) = Just (Right (Left f), args)
appAsFuncCall (ValP p args) = Just (Right (Right p), args)
appAsFuncCall _        = Nothing
