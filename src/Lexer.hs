module Lexer where

import           Control.Applicative (Alternative ((<|>)))
import           Data.Char           (isAlphaNum, isDigit, isLower, isSpace)
import           Parser              (Parser, charP, charPredP, eofP, manyPredP,
                                      somePredP, stringP, reservedP)

ifToken :: Parser String
ifToken = stringP "if"

thenToken :: Parser String
thenToken = stringP "then"

elseToken :: Parser String
elseToken = stringP "else"

fiToken :: Parser String
fiToken = stringP "fi"

letToken :: Parser String
letToken = stringP "let"

arrowToken :: Parser String
arrowToken = stringP "=>"

endToken :: Parser String
endToken = stringP "end"

entryToken :: Parser String
entryToken = stringP ">>>"

functionCallToken :: Parser Char
functionCallToken = charP '$'

declarationEndToken :: Parser Char
declarationEndToken = charP ';'

atomToken :: Parser String
atomToken = reservedP reservedToken camelToken
  where
    reservedToken = (ifToken <|> thenToken <|> elseToken <|> fiToken <|> letToken <|> endToken) <* ws'
    camelToken = (:) <$> charPredP error1 isLower <*> manyPredP error2 isAlphaNum
    error1 = "first character to be lower case"
    error2 = "literal to contain zero ore more alphanumeric characters"

integerToken :: Parser String
integerToken = somePredP error1 isDigit
  where
    error1 = "token to contain only digits"

plusToken :: Parser Char
plusToken = charP '+'

minusToken :: Parser Char
minusToken = charP '-'

productToken :: Parser Char
productToken = charP '*'

divisionToken :: Parser Char
divisionToken = charP '/'

equalToken :: Parser String
equalToken = stringP "=="

lessThanToken :: Parser Char
lessThanToken = charP '<'

lessThanEqualToken :: Parser String
lessThanEqualToken = stringP "<="

openParenthesis :: Parser Char
openParenthesis = charP '('

closedParenthesis :: Parser Char
closedParenthesis = charP ')'

ws :: Parser String
ws = manyPredP "zero or more whitespace characters" isSpace

ws' :: Parser String
ws' = somePredP "one ore more whitespace characters" isSpace

eofToken :: Parser String
eofToken = eofP
