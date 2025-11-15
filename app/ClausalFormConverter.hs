module ClausalFormConverter (convertToClausalForm) where

--module for converting a skolemized Formula into a ClausalForm

import Datatypes
import Data.Word

--get the unquantified inner Formula from a skolemized Formula
removeUniversalQuantifiers :: Formula -> Formula
removeUniversalQuantifiers f = 
    case f of
        ForAll _ g -> removeUniversalQuantifiers g
        _ -> f

--Tseitin's helper. Expects f: Iff(v g) where v is atomic and g is almost atomic.
-- i.e. g = And(p q), Or(p q), Not(p), Implies(p q), Iff(p q)
toCNF :: Formula -> ClausalForm
toCNF f = case f of
    Iff v g -> case v of
        Predicate nv tv -> case g of 
            And (Predicate np tp) (Predicate nq tq) -> 
                [[(Neg nv tv), (Pos np tp)], 
                [(Neg nv tv), (Pos nq tq)], 
                [(Neg np tp), (Neg nq tq), (Pos nv tv)]]
            
            Or (Predicate np tp) (Predicate nq tq) -> 
                [[(Neg np tp), (Pos nv tv)], 
                [(Neg nq tq), (Pos nv tv)], 
                [(Pos np tp), (Pos nq tq), (Neg nv tv)]]

            Not (Predicate np tp) -> 
                [[(Pos nv tv), (Pos np tp)],
                [(Neg nv tv), (Neg np tp)]]

            Implies (Predicate np tp) (Predicate nq tq) -> 
                [[(Pos nv tv), (Pos np tp)], 
                [(Pos nv tv), (Neg nq tq)], 
                [(Neg nv tv), (Neg np tp), (Pos nq tq)]]

            Iff (Predicate np tp) (Predicate nq tq) -> 
                [[(Pos np tp), (Pos nq tq), (Pos nv tv)], 
                [(Neg np tp), (Neg nq tq), (Pos nv tv)], 
                [(Neg nv tv), (Neg np tp), (Pos nq tq)], 
                [(Neg nv tv), (Neg nq tq), (Pos np tp)]]

            _ -> error "bad input: g not almost atomic"
        _ -> error "bad input: v not atomic"
    _ -> error "bad input: f != IFF(v g)"

{-
Perform Tseitin's Transformation on f to get a CNF formula.
We use "{[.]}" as names for new predicates because nothing can
have a name of this form as it would have broken the parser.
The argument range says what names can be given to new vars in
the current scope. Names are Word8 numbers (only 256 names!).
-}
tseitinRec :: Formula -> (Word8, Word8) -> (String, ClausalForm)
tseitinRec f (minName, maxName) = 
    let name = "{["++show maxName++"]}" in
    case f of
        Top -> 
            ("{[T]}", [])
        Bottom -> 
            ("{[B]}", [[Pos "{[p]}" [Object "{[o]}"]], [Neg "{[p]}" [Object "{[o]}"]]])
        Predicate np tp -> 
            ("{["++np++"]}", [[Pos np tp]])
        And x y -> case (x, y) of
            (Predicate np tp, Predicate nq tq) -> 
                ("{[]}", toCNF (Iff (Predicate name []) f))




            _ -> error ""
            
        _ -> error ""

--master tseitin func
tseitinsTransf :: Formula -> ClausalForm
tseitinsTransf f = 
    let (rootName, clauses) = tseitinRec f (0, -1) in
    [Pos ("{["++rootName++"]}") []] : clauses

{-
--does not use Tseitin's Transformation -> exponential increase in formula size
--also simplify things like And(True, x)
formulaToCNF :: Formula -> Int -> Formula
formulaToCNF f count = case f of
    Top                     -> f
    Bottom                  -> f
    Predicate _ _           -> f
    And x y ->  
        let x' = formulaToCNF x in
        let y' = formulaToCNF y in
        case (x', y') of
            (Top, z)    -> z
            (z, Top)    -> z
            (Bottom, _) -> Bottom
            (_, Bottom) -> Bottom
            _           -> And x' y'
    Or x y ->
        let x' = formulaToCNF x in
        let y' = formulaToCNF y in
        case (x', y') of
            (Top, _)    -> Top
            (_, Top)    -> Top
            (Bottom, z) -> z
            (z, Bottom) -> z
            _           -> Or x' y'
    Not g -> 
        case g of 
            Predicate _ _   -> Not g
            And x y         -> Or (formulaToCNF (Not x)) (formulaToCNF (Not y))

            
    Implies     -> Formula Formula
    Iff         -> Formula Formula
    ForAll      -> String Formula
    ThereExists -> String Formula
-}
{-
Assumes that the Formula is in the form
  (a | b | ...) & ... & (x | y | ...)
Note: in the Formula AST syntax, a clause can take 4 forms
  Or(Or(...), x)
  Or(x, Or(...))
  Or(Or(...), Or(...))
  Or(x,y)
And similarly for And in a CNF Formula AST
-}

convertToClausalForm :: Formula -> ClausalForm
convertToClausalForm f = tseitinsTransf (removeUniversalQuantifiers f)