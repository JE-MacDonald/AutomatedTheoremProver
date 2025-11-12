module Renamer (renameFormula) where
-- Intended to rename variables (a first step to turning into clausal form)
-- Does not handle an error of putting quantifiers inside of other quantifiers at the moment
-- Free variables get to keep their name as is
import Datatypes
import Data.Map (Map)
import qualified Data.Map as Map

rename :: Map String (Int, Bool) -> Term -> Term
rename dict (Variable name) = 
    case Map.lookup name dict of
        Just (count, True) -> Variable ((show count) ++ "_" ++ name)
        _ -> Variable name
rename dict (Function name terms ) = 
    Function name (map (rename dict) terms)
rename dict other =
    other

renameBinary :: (Formula -> Formula -> Formula) -> Formula -> Formula -> Map String (Int, Bool) -> (Map String (Int, Bool), Formula)
renameBinary t a b dict =
  let
    (dict1, newA) = renameVariables a dict
    (dict2, newB) = renameVariables b dict1
  in
    (dict2, t newA newB) 

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


renameVariables (ForAll name f) dict =
    let
        (count, _) = Map.findWithDefault (0, False) name dict
        newCount = count + 1
        newName = (show newCount) ++ "_" ++ name
        newDict = Map.insert name (newCount, True) dict
        (_, newF) = renameVariables f newDict
    in
        (Map.insert name (newCount, False) newDict, ForAll newName newF)
renameVariables (ThereExists name f) dict =
    let
        (count, _) = Map.findWithDefault (0, False) name dict
        newCount = count + 1
        newName = (show newCount) ++ "_" ++ name
        newDict = Map.insert name (newCount, True) dict
        (_, newF) = renameVariables f newDict
    in
        (Map.insert name (newCount, False) newDict, ThereExists newName newF)
renameVariables x y = (y, x)

renameFormula :: Formula -> Formula
renameFormula f = snd (renameVariables f Map.empty)