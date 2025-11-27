module Main where

import Datatypes
import Parser
import Renamer
import ClausalFormConverter
import qualified Text.Megaparsec as MP
import System.Environment (getArgs)
import FreeVariables (freeVariables)
import NegationNormalForm (nnfConverter)
import MoveQuantifiers (moveQuantifiers)
import Skolemize (skolemize)
import Resolution (resolution)

main :: IO ()
main = do
    --First-order resolution
    args <- getArgs
    case args of
        [path] -> do
            input <- readFile path
            case inputStringToFormula input of
                Left e -> putStrLn(MP.errorBundlePretty e)
                Right formula -> do 
                    let negatedFormula = Not formula
                    putStrLn "Negated Input Formula:"
                    print negatedFormula

                    let noFreeVars = freeVariables negatedFormula
                    putStrLn "\nStep 1: Remove Free Variables:"
                    print noFreeVars

                    let negNormForm = nnfConverter noFreeVars
                    putStrLn "\nStep 2a: Negation Normal Form:"
                    print negNormForm

                    let renamed = renameFormula negNormForm
                    putStrLn "\nStep 2b: Rename Quantified Variables:"
                    print renamed

                    let pnfFormula = moveQuantifiers renamed
                    putStrLn "\nStep 2c: Move Quantifiers (PNF):"
                    print pnfFormula

                    let skolemized = skolemize pnfFormula
                    putStrLn "\nStep 3: Skolemization:"
                    print skolemized

                    let clausalFormula = convertToClausalForm skolemized
                    putStrLn "\nSteps 4&5: CNF and Clausal Form:"
                    putStrLn (unlines (map show clausalFormula))

                    let result = resolution clausalFormula
                    if result
                        then putStrLn "Valid!"
                        else putStrLn "Invalid!"
        _ -> putStrLn "Error: Expects one input file!"
