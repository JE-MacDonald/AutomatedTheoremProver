module ClausalFormConverter (convertToCNF) where

import Datatypes

convertToCNF :: Formula -> ClausalFormula
convertToCNF _ = ClausalFormula [ Clause [Pos "p" [Variable "v"]] ]