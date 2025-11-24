module Renamer (renameFormula) where
import Datatypes
import Data.Map (Map)
import qualified Data.Map as Map

-- Does the actual renaming of variables in terms
-- If the term is a function then it recursively renames into the term
-- Parameters: Mapping, Input term
rename :: Map String Int -> Term -> Term
rename dict (Variable name) = 
    case Map.lookup name dict of
        Just count -> Variable ((show count) ++ "_" ++ name)
        _ -> Variable name
rename dict (Function name terms ) = 
    Function name (map (rename dict) terms)
rename _ other =
    other

-- Handles renaming binary formulae (such as And)
-- Parameters: Binary operation type, left formula, right formula, name list, mapping
renameBinary :: (Formula -> Formula -> Formula) -> Formula -> Formula -> Map String Int -> Map String Int -> (Map String Int, Formula)
renameBinary t a b nameDict dict =
  let
    (nameDict1, newA) = renameVariables a nameDict dict
    (nameDict2, newB) = renameVariables b nameDict1 dict
  in
    (nameDict2, t newA newB) 

-- Handles renaming quantifiers
-- Renames the quantified variable to (Number of times this variable name has been seen in quantifiers)_(Variable name)
-- Maintains a master list of all encountered variable names and its counts, as well as a local copy for mapping (must be local to respect scope)
-- Parameters: Quantifier type, name of variable quantified, formula, name list, mapping
renameQuantifier :: (String -> Formula -> Formula) -> String -> Formula -> Map String Int -> Map String Int -> (Map String Int, Formula)
renameQuantifier t name formula nameDict dict =
    let
        count = Map.findWithDefault 0 name nameDict
        newCount = count + 1
        newName = (show newCount) ++ "_" ++ name
        newDict = Map.insert name newCount dict
        newNameDict = Map.insert name newCount nameDict
        (finalNameDict, newF) = renameVariables formula newNameDict newDict
    in
        (finalNameDict, t newName newF)

-- The main renaming function for this file
-- Parameters: Formula to be renamed, dict of all found var names so far, Mapping (Name -> Times seen)
renameVariables :: Formula -> Map String Int -> Map String Int -> (Map String Int, Formula)
renameVariables (Predicate name terms) nameDict dict = (nameDict, Predicate name (map (rename dict) terms))

renameVariables (Not a) nameDict dict =
    let 
        (newDict, newA) = renameVariables a nameDict dict
    in
        (newDict, Not newA)

renameVariables (And p q) nameDict dict =
  renameBinary And p q nameDict dict

renameVariables (Or p q) nameDict dict =
  renameBinary Or p q nameDict dict

renameVariables (Implies p q) nameDict dict =
  renameBinary Implies p q nameDict dict

renameVariables (Iff p q) nameDict dict =
  renameBinary Iff p q nameDict dict

renameVariables (ForAll name f) nameDict dict = renameQuantifier ForAll name f nameDict dict
renameVariables (ThereExists name f) nameDict dict = renameQuantifier ThereExists name f nameDict dict

renameVariables x y _ = (y, x)

-- The actual function that should be called for Step 2b
renameFormula :: Formula -> Formula
renameFormula f = snd (renameVariables f Map.empty Map.empty)