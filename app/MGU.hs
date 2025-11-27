module MGU (mgu) where

import Datatypes
import qualified Data.Map as Map


--Helpers for mgu
noPolarity :: Literal -> (String, [Term])
noPolarity (Pos s terms) = (s, terms)
noPolarity (Neg s terms) = (s, terms)

--Check if first term occurs in second term.
occurs :: Term -> Term -> Bool
occurs t1 t2 = case t2 of
    Function _ terms -> any (occurs t1) terms
    _ 
        | t1 == t2 -> True
        | otherwise -> False

--Substitutes t2 into all occurrences of t1 found in t3
--Note that this technically works for substituting any term to any term,
--but I use it only for substituting variable to terms
tsubterm :: (Term, Term) -> Term -> Term
tsubterm (t1, t2) t3 
    | t1 == t3 = t2
    | otherwise = case t3 of
        Function f terms -> Function f (map (tsubterm (t1, t2)) terms)
        _ -> t3

--Applies tsubterm to both terms in input tuple
tsub :: (Term, Term) -> (Term, Term) -> (Term, Term)
tsub (t1, t2) (lt1, lt2) = (tsubterm (t1, t2) lt1, tsubterm (t1, t2) lt2)

--The main meat of the M-M algorithm
--Input is a list of equations (split into two) represented as Term, Term tuple
--Splitting into two is necessary because the entire list needs to be seen at every step of the recursion.
--List of equations becomes Nothing if the literals cannot be unified
--Performs special transformations on the list of equations.
mguMain :: Maybe ([(Term, Term)], [(Term, Term)]) -> Maybe [(Term, Term)]
mguMain Nothing = Nothing
mguMain (Just (front,[])) = Just front
mguMain (Just (front, (t1, t2):rest)) 
    | t1 == t2 = mguMain $ Just (front, rest) --equality transformation
    | otherwise = case (t1, t2) of 
        (Object _, Variable _) -> mguMain $ Just ((t2, t1):front, rest) --reordering transformation
        (Function _ _, Variable _) -> mguMain $ Just ((t2, t1):front, rest)
        (Function n1 terms1, Function n2 terms2) --term reduction transformation
            | n1 /= n2 -> Nothing
            | length terms1 /= length terms2 -> Nothing
            | otherwise -> mguMain $ Just (front, (zip terms1 terms2) ++ rest)
        (Variable _, _) -> case occurs t1 t2 of --occurs check + variable elimination
            True -> Nothing
            False -> mguMain $ Just ((t1, t2):(map (tsub (t1, t2)) front), map (tsub (t1, t2)) rest)
        _ -> Nothing --mismatched function symbols (object constant with 0-ary function)


--Iterates mguMain repeatedly until an mgu is found or not.
--Transformations continue until either fail condition or the result is in solved form.
mguLoop :: Maybe [(Term, Term)] -> Maybe [(Term, Term)]
mguLoop Nothing = Nothing
mguLoop (Just equations) = case mguMain $ Just ([], equations) of
    Nothing -> Nothing
    Just newEqs
        | reverse newEqs == equations -> Just newEqs
        | otherwise -> mguLoop $ Just newEqs

--Convert variable term into string type
--Must be called on solved form (which it is)
stringify :: [(Term, Term)] -> [(String, Term)]
stringify [] = []
stringify ((Variable name, t):rest) = (name, t):stringify rest
stringify _ = [] --if not called on solved form returns empty list


--Martelli-Montanari algorithm for finding mgu between two literals
--Expects two literals and returns a Substitution dictionary.
--Will find mgu to unify the literals ignoring polarity (if possible).
mgu :: Literal -> Literal -> Subst
mgu l1 l2
    | s1 /= s2 = Nothing --predicate symbols don't match
    | length t1 /= length t2 = Nothing --arity of predicates don't match
    | otherwise = case mguLoop (Just (zip t1 t2)) of
        Nothing -> Nothing
        Just sub -> Just $ Map.fromList $ stringify sub
    where 
        (s1, t1) = noPolarity l1
        (s2, t2) = noPolarity l2