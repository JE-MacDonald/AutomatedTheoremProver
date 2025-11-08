module Parser where

import Formula
import Text.Megaparsec
import Text.Megaparsec.Char

data Parser
    = Top