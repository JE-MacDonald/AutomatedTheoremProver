module Datatypes where

data Term
    = Variable String
    | Object String
    | Function String [Term]
    deriving (Eq)

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
    deriving (Eq)

--Clausal Form types
data Literal 
    = Pos String [Term]  --predicate
    | Neg String [Term]  --not(predicate)
type Clause = [Literal]
type ClausalForm = [Clause]


-- Neater prints, not required though
instance Show Term where
    show (Variable name) = name
    show (Object name) = name
    show (Function name terms) = name ++ (show terms)

instance Show Formula where
    show (Top) = "True"
    show (Bottom) = "False"
    show (Predicate name terms) = name ++ (show terms)
    show (And a b) = "(" ++ (show a) ++ ") ∧ (" ++ (show b) ++ ")"
    show (Or a b) = "(" ++ (show a) ++ ") ∨ (" ++ (show b) ++ ")"
    show (Implies a b) = "(" ++ (show a) ++ ") -> (" ++ (show b) ++ ")"
    show (Iff a b) = "(" ++ (show a) ++ ") <-> (" ++ (show b) ++ ")"
    show (ForAll name a) = "∀ " ++ ( name) ++ ". (" ++ (show a) ++ ")"
    show (ThereExists name a) = "∃ " ++ ( name) ++ ". (" ++ (show a) ++ ")"
    show (Not a) = "¬(" ++ (show a) ++ ")"

instance Show Literal where
    show (Pos name terms) = name ++ (show terms)
    show (Neg name terms) = "¬" ++ name ++ (show terms)