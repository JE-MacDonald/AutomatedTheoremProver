module FreeVariables (freeVariables) where

-- module for existentially quantifying all free variables in a formula

import Datatypes
import qualified Data.Set as Set

type VarSet = Set.Set String

-- Helper function to find the set of all free variables in a formula
-- A variable is free if it is not bound by a quantifier
findFreeVars :: Formula -> VarSet
findFreeVars = go Set.empty
  where
    go :: VarSet -> Formula -> VarSet
    go _ Top    = Set.empty
    go _ Bottom = Set.empty

    -- Finds all free variables in a predicate's terms
    go bound (Predicate _ terms) = Set.unions (map (freeVarsInTerm bound) terms)

    go bound (Not p)           = go bound p
    -- A variable is free in a logical connective if it is free in p or q
    go bound (And p q)         = go bound p `Set.union` go bound q
    go bound (Or p q)          = go bound p `Set.union` go bound q
    go bound (Implies p q)     = go bound p `Set.union` go bound q
    go bound (Iff p q)         = go bound p `Set.union` go bound q

    -- A quantifier's variable becomes bound
    go bound (ForAll x p)      = go (Set.insert x bound) p
    go bound (ThereExists x p) = go (Set.insert x bound) p

    freeVarsInTerm :: VarSet -> Term -> VarSet
    -- A variable is free if it is not bound
    freeVarsInTerm bound (Variable x)
        | Set.member x bound = Set.empty
        | otherwise          = Set.singleton x
    -- An object does not have variables
    freeVarsInTerm _     (Object _)         = Set.empty
    -- Finds free variables in a function term
    freeVarsInTerm bound (Function _ terms) = Set.unions (map (freeVarsInTerm bound) terms)

-- Existentially quantifies all free variables in the formula
freeVariables :: Formula -> Formula
freeVariables f =
    let vars = Set.toList (findFreeVars f)
    in foldr ThereExists f vars
