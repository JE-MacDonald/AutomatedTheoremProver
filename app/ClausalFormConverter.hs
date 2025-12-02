module ClausalFormConverter (convertToClausalForm) where

--module for converting a skolemized Formula into a ClausalForm

import Datatypes
import Data.Word
import qualified Data.Bits as Bit (shiftR)

--get the unquantified inner Formula from a skolemized Formula
--and get a list of the quantified variables
removeUniversalQuantifiers :: Formula -> [Term] -> (Formula, [Term])
removeUniversalQuantifiers f qvars = 
    case f of
        ForAll x g -> removeUniversalQuantifiers g (Variable x : qvars)
        _ -> (f, qvars)

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
Use numbers as names because user can only use alphabet strings.
The argument range says what names can be given to new vars in
the current scope. Names are Word16 numbers (2^16 possible names).
-}
tseitinRec :: Formula -> [Term] -> (Word16, Word16) -> (String, [Term], ClausalForm)
tseitinRec f qvars (minName, maxName) = 
    let name = show minName in
    case f of
        Top ->
            ("+", [], [[Pos "+" []]])

        Bottom ->
            ("-", [], [[Neg "-" []]])

        Predicate np tp -> 
            (np, tp, [])

        And x y -> 
            let mid = minName + Bit.shiftR (maxName - minName) 1 in
            let (nx, tx, cx) = tseitinRec x qvars (minName+1, mid) in
            let (ny, ty, cy) = tseitinRec y qvars (mid+1, maxName) in
            let p = Predicate nx tx in
            let q = Predicate ny ty in
            (name, qvars, toCNF (Iff (Predicate name qvars) (And p q) ) ++ cx ++ cy)

        Or x y -> 
            let mid = minName + Bit.shiftR (maxName - minName) 1 in
            let (nx, tx, cx) = tseitinRec x qvars (minName+1, mid) in
            let (ny, ty, cy) = tseitinRec y qvars (mid+1, maxName) in
            let p = Predicate nx tx in
            let q = Predicate ny ty in
            (name, qvars, toCNF (Iff (Predicate name qvars) (Or p q) ) ++ cx ++ cy)

        Not x ->
            let (nx, tx, cx) = tseitinRec x qvars (minName+1, maxName) in
            let p = Predicate nx tx in
            (name, qvars, toCNF (Iff (Predicate name qvars) (Not p) ) ++ cx)

        Implies x y -> 
            let mid = minName + Bit.shiftR (maxName - minName) 1 in
            let (nx, tx, cx) = tseitinRec x qvars (minName+1, mid) in
            let (ny, ty, cy) = tseitinRec y qvars (mid+1, maxName) in
            let p = Predicate nx tx in
            let q = Predicate ny ty in
            (name, qvars, toCNF (Iff (Predicate name qvars) (Implies p q) ) ++ cx ++ cy)

        Iff x y ->
            let mid = minName + Bit.shiftR (maxName - minName) 1 in
            let (nx, tx, cx) = tseitinRec x qvars (minName+1, mid) in
            let (ny, ty, cy) = tseitinRec y qvars (mid+1, maxName) in
            let p = Predicate nx tx in
            let q = Predicate ny ty in
            (name, qvars, toCNF (Iff (Predicate name qvars) (Iff p q) ) ++ cx ++ cy )

        _ -> error "Unexpected Quantifier"

--master tseitin func
tseitinsTransf :: (Formula, [Term]) -> ClausalForm
tseitinsTransf (f, qvars) = 
    let (rootName, rootTerms, clauses) = tseitinRec f qvars (0, -1) in
    [Pos rootName rootTerms] : clauses

convertToClausalForm :: Formula -> ClausalForm
convertToClausalForm f = tseitinsTransf (removeUniversalQuantifiers f [])