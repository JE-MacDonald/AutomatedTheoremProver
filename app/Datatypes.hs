module Datatypes where

data Term
    = Variable String
    | Object String
    | Function String [Term]
    deriving (Eq, Show)

data Formula
    = Top     --true
    | Bottom  --false
    | Predicate String [Term]
    | And Formula Formula
    | Or Formula Formula
    | Not Formula
    | Implies Formula Formula
    | Iff Formula Formula
    | ForAll String Formula
    | ThereExists String Formula
    deriving (Eq, Show)

--Clausal Form types
data Literal 
    = Pos String [Term]  --predicate
    | Neg String [Term]  --not(predicate)
newtype Clause = Clause [Literal]
newtype ClausalFormula = ClausalFormula [Clause]
