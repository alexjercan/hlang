module Lexer where

import           Data.Char
import           Parser

letToken :: Parser String
letToken = stringP "let"

arrowToken :: Parser String
arrowToken = stringP "=>"

endToken :: Parser String
endToken = stringP "end"

literalToken :: Parser String
literalToken = (:) <$> charPredP isLower <*> manyPredP isAlphaNum

integerToken :: Parser String
integerToken = somePredP isDigit

plusToken :: Parser Char
plusToken = charP '+'

multiplicationToken :: Parser Char
multiplicationToken = charP '*'

openParenthesis :: Parser Char
openParenthesis = charP '('

closedParenthesis :: Parser Char
closedParenthesis = charP ')'

ws :: Parser String
ws = manyPredP isSpace
