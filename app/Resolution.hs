module Resolution (resolution) where

import Control.Monad (foldM)
import Data.List (delete, nub, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Datatypes


resolution :: ClausalForm -> Bool
resolution _ = True
--Given the list of clauses in clausal form, do resolution logic.
--Start by ensuring variables are distinct in each clause. (No shared variables in separate clauses -> slide 38 in 7-Resolution.pdf)
--For unification, can use Robinson's algorithm, or Martelli and Montanari algorithm, etc.
--Create as many helper functions as needed. At the end, should return True if an empty clause is derived ("Valid!" in Main.hs)
--If no empty clause could be found or all possibilities exhausted, returns False ("Invalid!" in Main.hs)
--Because resolution is refutation-complete and not decidable, program may run/loop forever. This is expected.
--When making a helper function, you can briefly describe the functionality and leave it blank for me to do that part, etc.