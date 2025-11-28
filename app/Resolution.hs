module Resolution (resolution) where

import Datatypes
import MGU (mgu)
import qualified Data.Map as Map
import Data.List (nub, sort, find)
import Data.Maybe (isJust)

-- Given the list of clauses in clausal form, do resolution logic.
-- Start by ensuring variables are distinct across clauses.
-- For unification, we use Martelli-Montanari algorithm (MGU.hs)
-- Binary resolution inference rule: two first-order literals are complementary if one unifies with the negation of the other.
-- First-order factoring reduces two literals to one if they are unifiable.
-- The combination of binary resolution and factoring is complete.
-- Tautology reduction: If a clause contains a tautology like p or ~p, we can remove this clause as it is trivially True.
-- Returns True if an empty clause is derived ("Valid!" in Main.hs)
-- If no empty clause could be found or all possibilities exhausted, returns False ("Invalid!" in Main.hs)
-- Because resolution is refutation-complete and not decidable, program may run/loop forever. This is expected.
resolution :: ClausalForm -> Bool
resolution clausalform = search cleanedclauses
  where cleanedclauses = removeTautologies (map factorClause (standardizeAll clausalform))

-- search repeatedly applies resolution until either the empty clause is found or no new clauses can be generated
search :: ClausalForm -> Bool
search clauses =
  let
    resolvents = nub (concat [ resolvePair c1 c2 | (c1, c2) <- distinctPairs clauses ])
    newClauses = filter (not . isTautology) resolvents
    novel = filter (`notElem` clauses) newClauses
  in
    any null newClauses || (not (null novel) && search (clauses ++ novel))

-- Generates all distinct pairs of elements from a list. ie. [1,2,3] -> [(1,2),(1,3),(2,3)].
-- This is to ensure that we don't accidentally resolve a clause with itself. The resolution rule must take a positive literal from a clause and a negative literal from a different clause.
distinctPairs :: [a] -> [(a,a)]
distinctPairs [] = []
distinctPairs (x:xs) = [(x,y) | y <- xs] ++ distinctPairs xs

-- Walks through all clauses in the clausal form, giving each one a unique index and renaming its variables so they don’t clash with variables from other clauses.
standardizeAll :: ClausalForm -> ClausalForm
standardizeAll clauses = [ standardizeClause clause n | (clause, n) <- zip clauses [1..] ]

-- Renames all literals in a clause by tagging their variables with the clause’s unique number.
standardizeClause :: Clause -> Int -> Clause
standardizeClause clause n = map (renameLiteral n) clause

-- Renames all the terms inside a literal by applying the clause’s unique number to its variables.
renameLiteral :: Int -> Literal -> Literal
renameLiteral n (Pos name terms) = Pos name (map (renameTerm n) terms)
renameLiteral n (Neg name terms) = Neg name (map (renameTerm n) terms)

-- Actually adds the clause index to the variables and terms
renameTerm :: Int -> Term -> Term
renameTerm n (Variable v) = Variable (v ++ "_" ++ show n)
renameTerm n (Function f terms) = Function f (map (renameTerm n) terms)
renameTerm _ term = term

-- Tries to resolve two clauses by finding complementary literals that unify, removing them, applying the substitution, and returning the new resolvent(s).
resolvePair :: Clause -> Clause -> [Clause]
resolvePair c1 c2 = concatMap tryResolve [(l, m) | l <- c1, m <- c2]
  where tryResolve (l, m) =
         case mgu l m of
         Just subst | isComplementary l m -> [ factorClause (normalizeClause (applyClause subst (remove l c1 ++ remove m c2))) ]
         _ -> []

-- Remove the first x that we find in the list. ie. x=3 in [1,2,3,4] => [1,2,4]. Works for inferred data types.
remove :: Eq a => a -> [a] -> [a]
remove x list = case list of
  [] -> []
  (l1:ls) -> if x == l1
              then ls
              else l1 : remove x ls

-- Mainly removes duplicates but also sorts.
normalizeClause :: Clause -> Clause
normalizeClause clause = sort (nub clause)

-- If two literals in the same clause share the same sign and are unifiable, then we apply that unifier on the whole clause and drop one of the literals via normalizeClause to reduce redundancy.
factorClause :: Clause -> Clause
factorClause clause =
    case findFactor clause of
        Nothing -> clause
        Just subst -> factorClause (normalizeClause (applyClause subst clause))

-- When two literals in a clause share the same sign and are unifiable, then return the mgu for these two literals.
findFactor :: Clause -> Maybe (Map.Map String Term)
findFactor [] = Nothing
findFactor (l1:ls) =
    case find (\l2 -> sameSign l1 l2 && isJust (mgu l1 l2)) ls of
        Just l2 -> mgu l1 l2
        Nothing -> findFactor ls

-- Returns true if the two literals have the same sign
sameSign :: Literal -> Literal -> Bool
sameSign (Pos _ _) (Pos _ _) = True
sameSign (Neg _ _) (Neg _ _) = True
sameSign _ _ = False

-- If a clause in the clausal form contains a tautology, remove that clause from the clausal form.
removeTautologies :: ClausalForm -> ClausalForm
removeTautologies clauses = [clause | clause <- clauses, not (isTautology clause)]

-- A clause is a tautology if any pair of literals in the clause is complementary and unifiable.
isTautology :: Clause -> Bool
isTautology clause = any (\(l1, l2) -> isComplementary l1 l2 && isJust (mgu l1 l2)) [ (l1, l2) | l1 <- clause, l2 <- clause ]

-- Returns true when two literals share the same name and have opposite sign: This means that if they are unifiable, then they are a tautology.
isComplementary :: Literal -> Literal -> Bool
isComplementary (Pos p _) (Neg q _) = p == q
isComplementary (Neg p _) (Pos q _) = p == q
isComplementary _ _ = False

-- Applies a substitution to every literal in a clause.
applyClause :: Map.Map String Term -> Clause -> Clause
applyClause subst = map (applySubst subst)

-- Applies a substitution to all the terms inside a single literal.
applySubst :: Map.Map String Term -> Literal -> Literal
applySubst subst (Pos s ts) = Pos s (map (applyTerm subst) ts)
applySubst subst (Neg s ts) = Neg s (map (applyTerm subst) ts)

-- Applies a substitution to a term, replacing variables with their mapped terms and recursing through functions.
applyTerm :: Map.Map String Term -> Term -> Term
applyTerm subst (Variable v) =
    case Map.lookup v subst of
        Just t  -> t
        Nothing -> Variable v
applyTerm subst (Function f ts) = Function f (map (applyTerm subst) ts)
applyTerm _ t = t