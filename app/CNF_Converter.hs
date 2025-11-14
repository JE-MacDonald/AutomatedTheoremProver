module CNF_Converter (convertToCNF) where

import Datatypes

convertToCNF :: Formula -> ClausalFormula
convertToCNF _ = ClausalFormula [ Clause [Pos "p" [Variable "v"]] ]