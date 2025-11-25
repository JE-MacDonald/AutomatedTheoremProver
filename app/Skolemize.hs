module Skolemize (skolemize) where
-- This module assumes that renamer, NNF, and move quantifiers modules have been called
-- so it does not support implies or iff

-- module for producing an equisat formula without existential quantifiers (skolemization)

import Datatypes

skolemize :: Formula -> Formula
skolemize f = fst (go f [] 0)


-- Helper function for skolemization
-- Parameters:
--  formula to skolemize, 
--  list of universally quantified variables in scope, 
--  count of current skolem functions
go :: Formula -> [String] -> Int -> (Formula, Int)

go (And p q) uni n =
    let 
        (p1, n1) = go p uni n
        (q1, n2) = go q uni n1
    in
        (And p1 q1, n2)

go (Or p q) uni n =
    let 
        (p1, n1) = go p uni n
        (q1, n2) = go q uni n1
    in
        (Or p1 q1, n2)

go (Not p) uni n =
    let
        (p1, n1) = go p uni n
    in
        (Not p1, n1)

go (ForAll x p) uni n =
    let
        (p1, n1) = go p (uni ++ [x]) n
    in
        (ForAll x p1, n1)

-- Handles existential quantifiers by skolemizing them:
-- There exists an x such that p is inside the scope of universally quantified variables uni.
-- Replace all x with a skolem function of uni, 
-- named skn here n is the count of current skolem functions.
-- Then the existential quantifier is removed and skolemization continues.
go (ThereExists x p) uni n =
    let
        skolemName = "sk" ++ show n
        skolemFunc = Function skolemName (map Variable uni)
        pSub       = substitute x skolemFunc p
    in
        go pSub uni (n + 1)

-- Handles the rest
go x _ n = (x, n)

-- Replaces variable y with function term f in formula
substitute :: String -> Term -> Formula -> Formula

substitute y f (Predicate name terms) = Predicate name (map (subTerm y f) terms)

substitute y f (And p q) = And (substitute y f p) (substitute y f q)
substitute y f (Or p q)  = Or (substitute y f p) (substitute y f q)
substitute y f (Not p)   = Not (substitute y f p)

-- y is not free if it is bound by the quantifier
substitute y f (ForAll x p)
    | x == y    = ForAll x p
    | otherwise = ForAll x (substitute y f p)

substitute y f (ThereExists x p)
    | x == y    = ThereExists x p
    | otherwise = ThereExists x (substitute y f p)

-- Handles the rest
substitute _ _ x = x

-- Substitutes in terms
subTerm :: String -> Term -> Term -> Term
subTerm y f (Variable x)
    | x == y    = f
    | otherwise = Variable x
subTerm _ _ (Object o) = Object o
subTerm y f (Function name terms) = Function name (map (subTerm y f) terms)
