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

    --test for ClausalFormConverter
    input <- readFile "input2.txt"
    case inputStringToFormula input of
        Left e -> putStrLn(MP.errorBundlePretty e)
        Right formula -> do 
            let q = renameFormula formula
            let x = convertToClausalForm q
            print (show q)
            putStrLn ""
            putStrLn (unlines (map show x))

