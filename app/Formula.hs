module Formula where

data Term
    = Variable String
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

--maybe add a clausal type for clausal form? this may be easier to work with 
--when doing resolution refutations. otherwise we have hellish things:
--Or( And(And(...And(xyz, pqr)), abc) , Or( And(And(...And(xyz, pqr)), abc), ...Or(AND , AND))
-- but maybe that's just ugly to read and the computer loves it, idk