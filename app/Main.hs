module Main where

import Formula
import Parser

main :: IO ()
main = do
    --go thru the steps of FO resolution and then refutation by calling
    --funcs from modules (ie, PNF conversion module, skolem module, etc)
    let x = Top :: Formula
    print x
