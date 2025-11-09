module Parser (inputStringToFormula) where

--Parser module for converting formula strings into Formulas

import Datatypes
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Data.Void as Void
import qualified Control.Applicative as CA

type Parser = MP.Parsec Void.Void String

--for passing white space in text (' ', '\n', etc.)
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space Char.space1 CA.empty CA.empty

--parse a particular string
pString :: String -> Parser String
pString str = Char.string str <* spaceConsumer

--parse inside () (parens)
pParens :: Parser ty -> Parser ty
pParens = MP.between (pString "(") (pString ")")

--parse inside [] (brackets)
pBrackets :: Parser ty -> Parser ty
pBrackets = MP.between (pString "[") (pString "]")

--parse inside {} (braces)
pBraces :: Parser ty -> Parser ty
pBraces = MP.between (pString "{") (pString "}")

--parse a name for a variable, object, function, or predicate
pName :: Parser String
pName = CA.some Char.letterChar <* spaceConsumer

--parse a term
pTerm :: Parser Term
pTerm = do
    keyword <- CA.some Char.letterChar <* spaceConsumer
    case keyword of
        "VAR"       -> Variable <$> pBrackets pName
        "OBJ"       -> Object <$> pBrackets pName
        "FUNC"      -> Function <$> pBrackets pName <*> pParens pTermSequence
        _           -> fail $ "Bad term keyword: " ++ keyword

--parse a sequence of terms for a function or predicate
pTermSequence :: Parser [Term]
pTermSequence = MP.many pTerm

--parse a formula
pFormula :: Parser Formula
pFormula = do
    keyword <- CA.some Char.letterChar <* spaceConsumer
    case keyword of
        "TRUE"      -> return Top

        "FALSE"     -> return Bottom

        "PRED"      -> Predicate <$> pBrackets pName <*> pParens pTermSequence
        
        "AND"       -> do 
            (f1, f2) <- pParens ((,) <$> pFormula <*> pFormula)
            return (And f1 f2)

        "OR"        -> do 
            (f1, f2) <- pParens ((,) <$> pFormula <*> pFormula)
            return (Or f1 f2)

        "NOT"       -> Not <$> pParens pFormula

        "IMPLIES"   -> do 
            (f1, f2) <- pParens ((,) <$> pFormula <*> pFormula)
            return (Implies f1 f2)

        "IFF"       -> do 
            (f1, f2) <- pParens ((,) <$> pFormula <*> pFormula)
            return (Iff f1 f2)

        "A"         -> ForAll <$> pBraces pName <*> pParens pFormula

        "E"         -> ThereExists <$> pBraces pName <*> pParens pFormula
        
        _           -> fail $ "Bad formula keyword: " ++ keyword
           
--public funtion that converts a well-formed formula strings into a Formula
-- and fails with an error otherwise
inputStringToFormula :: String -> Either (MP.ParseErrorBundle String Void.Void) Formula 
inputStringToFormula = MP.parse pFormula ""

