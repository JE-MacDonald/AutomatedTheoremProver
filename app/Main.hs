module Main where

import Datatypes
import Parser
import Renamer
import ClausalFormConverter
import qualified Text.Megaparsec as MP

main :: IO ()
main = do
    --go thru the steps of FO resolution and then refutation by calling
    --funcs from modules (ie, PNF conversion module, skolem module, etc)

    --test for Parser.hs
    input <- readFile "input.txt"
    case inputStringToFormula input of
        Left e -> putStrLn(MP.errorBundlePretty e)
        Right formula -> do 
            let x = formula :: Formula --just messing with syntax
            print (renameFormula x)
