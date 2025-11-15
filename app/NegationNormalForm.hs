module NegationNormalForm (nnfConverter) where
import Datatypes

-- Only used when it SHOULD negate
-- Parameters: Outputted type, (left formula, should negate), (right formula, should negate)
nnfBinary :: (Formula -> Formula -> Formula) -> (Formula, Bool) -> (Formula, Bool) -> Formula
nnfBinary t (a, negateA) (b, negateB) =
    t (nnfHelper a negateA) (nnfHelper b negateB)

-- Parameters: formula to do NNF on, whether it should negate or not
nnfHelper :: Formula -> Bool -> Formula
nnfHelper Top True = Bottom
nnfHelper Top False = Top
nnfHelper Bottom True = Top
nnfHelper Bottom False = Bottom

nnfHelper (Predicate name terms) True =
    Not (Predicate name terms)
nnfHelper (Predicate name terms) False =
    Predicate name terms

nnfHelper (And a b) True = nnfBinary Or (a, True) (b, True)
nnfHelper (And a b) False = nnfBinary And (a, False) (b, False)

nnfHelper (Or a b) True = nnfBinary And (a, True) (b, True)
nnfHelper (Or a b) False = nnfBinary Or (a, False) (b, False)

nnfHelper (Implies a b) True = nnfBinary And (a, False) (b, True)
nnfHelper (Implies a b) False = nnfBinary Or (a, True) (b, False)

nnfHelper (Iff a b) True = Or (nnfBinary And (a, False) (b, True)) (nnfBinary And (a, True) (b, False))
nnfHelper (Iff a b) False = Or (nnfBinary And (a, True) (b, True)) (nnfBinary And (a, False) (b, False))

nnfHelper (Not f) negate = nnfHelper f (not negate)

nnfHelper (ForAll name f) True = ThereExists name (nnfHelper f True)
nnfHelper (ForAll name f) False = ForAll name (nnfHelper f False)

nnfHelper (ThereExists name f) True = ForAll name (nnfHelper f True)
nnfHelper (ThereExists name f) False = ThereExists name (nnfHelper f False)

nnfConverter :: Formula -> Formula
nnfConverter formula = nnfHelper formula False
