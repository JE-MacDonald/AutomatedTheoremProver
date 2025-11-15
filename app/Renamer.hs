module Renamer (renameFormula) where
import Datatypes
import Data.Map (Map)
import qualified Data.Map as Map

-- Does the actual renaming of variables in terms
-- If the term is a function then it recursively renames into the term
-- Parameters: Mapping, Input term
rename :: Map String (Int, Bool) -> Term -> Term
rename dict (Variable name) = 
    case Map.lookup name dict of
        Just (count, True) -> Variable ((show count) ++ "_" ++ name)
        _ -> Variable name
rename dict (Function name terms ) = 
    Function name (map (rename dict) terms)
rename _ other =
    other

-- Handles renaming binary formulae (such as And)
-- Parameters: Binary operation type, left formula, right formula, mapping
renameBinary :: (Formula -> Formula -> Formula) -> Formula -> Formula -> Map String (Int, Bool) -> (Map String (Int, Bool), Formula)
renameBinary t a b dict =
  let
    (dict1, newA) = renameVariables a dict
    (dict2, newB) = renameVariables b dict1
  in
    (dict2, t newA newB) 

-- Handles renaming quantifiers
-- Renames the quantified variable to (Number of times this variable name has been seen in quantifiers)_(Variable name)
-- Keeps track of if the variable is quantified, and if it has already been quantified before the call it errors
-- Parameters: Quantifier type, name of variable quantified, formula, mapping
renameQuantifier :: (String -> Formula -> Formula) -> String -> Formula -> Map String (Int, Bool) -> (Map String (Int, Bool), Formula)
renameQuantifier t name formula dict =
    let
        (count, _) = Map.findWithDefault (0, False) name dict
        newCount = count + 1
        newName = (show newCount) ++ "_" ++ name
        newDict = Map.insert name (newCount, True) dict
        (_, newF) = renameVariables formula newDict
    in
        case Map.lookup name dict of
          Just (_, True) -> error ("Variable " ++ name ++ " quantified twice in the same scope!")
          _ -> (Map.insert name (newCount, False) newDict, t newName newF)

-- The main renaming function for this file
-- Parameters: Formula to be renamed, Mapping (Name -> (Times seen, isQuantified))
renameVariables :: Formula -> Map String (Int, Bool)-> (Map String (Int, Bool), Formula)
renameVariables (Predicate name terms) dict = (dict, Predicate name (map (rename dict) terms))

renameVariables (Not a) dict =
    let 
        (newDict, newA) = renameVariables a dict
    in
        (newDict, Not newA)

renameVariables (And p q) dict =
  renameBinary And p q dict

renameVariables (Or p q) dict =
  renameBinary Or p q dict

renameVariables (Implies p q) dict =
  renameBinary Implies p q dict

renameVariables (Iff p q) dict =
  renameBinary Iff p q dict


renameVariables (ForAll name f) dict = renameQuantifier ForAll name f dict
renameVariables (ThereExists name f) dict = renameQuantifier ThereExists name f dict

-- Swaps the input because the return type is a tuple of the inputs reversed pretty much
renameVariables x y = (y, x)

-- The actual function that should be called for Step 2b
renameFormula :: Formula -> Formula
renameFormula f = snd (renameVariables f Map.empty)