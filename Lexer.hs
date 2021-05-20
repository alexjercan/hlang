module Lexer where

import           Data.Char
import           Parser

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

literalToken :: Parser String
literalToken = (:) <$> charPredP isLower <*> manyPredP isAlphaNum

integerToken :: Parser String
integerToken = somePredP isDigit

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
ws = manyPredP isSpace

ws' :: Parser String
ws' = somePredP isSpace