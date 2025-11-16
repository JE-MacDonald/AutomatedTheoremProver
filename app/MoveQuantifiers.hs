module MoveQuantifiers (moveQuantifiers) where
import Datatypes
import Data.Tree 
-- This module assumes both renamer & NNF modules have been called
-- so they do not support things such as negation, implies, and iff

-- Gathers all of the quantifiers into a tree (to keep scoping) and removes the quantifiers while it's at it
-- An anonymous function is used in some instances because the tree datatype requires a node value
-- Parameters: formula to get the quantifiers from
gatherQuantifiers :: Formula -> (Formula, Tree (Formula -> Formula))
gatherQuantifiers (And a b) =
    let
        (a', aTree) = gatherQuantifiers a
        (b', bTree) = gatherQuantifiers b
    in
        (And a' b', Node (\x -> x) [aTree, bTree])

gatherQuantifiers (Or a b) =
    let
        (a', aTree) = gatherQuantifiers a
        (b', bTree) = gatherQuantifiers b
    in
        (Or a' b', Node (\x -> x) [aTree, bTree])

gatherQuantifiers (ForAll name f) =
    let
        (f', tree) = gatherQuantifiers f
    in
        (f', Node (ForAll name) [tree])

gatherQuantifiers (ThereExists name f) =
    let
        (f', tree) = gatherQuantifiers f
    in
        (f', Node (ThereExists name) [tree])


-- The rest just return themselves (It's assumed it'll be fine due to the preconditions outlined above)
gatherQuantifiers f = (f, Node (\x -> x) [])


-- Turns the quantifiers into a tree then flattens that tree (flatten SHOULD keep ordering & scoping)
-- Then the quantifiers are just added to the front using fold
moveQuantifiers :: Formula -> Formula
moveQuantifiers f =
    let 
        (f', quantifiers) = gatherQuantifiers(f)
        list = flatten quantifiers
    in
        foldr (\x accum -> x accum) f' list

