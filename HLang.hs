module HLang where

import           Control.Applicative
import           Lexer
import           Parser

data Factor
  = Atom String
  | Integer Int
  | Parenthesis Expression
  deriving (Show, Eq)

atom :: Parser Factor
atom = Atom <$> literalToken

integer :: Parser Factor
integer = Integer . read <$> integerToken

parenthesis :: Parser Factor
parenthesis = openParenthesis *> ws *> (Parenthesis <$> expression) <* ws <* closedParenthesis

factor :: Parser Factor
factor = atom <|> integer <|> parenthesis

data Term
  = Multiplication Factor Term
  | Factor Factor
  deriving (Show, Eq)

multiplication :: Parser Term
multiplication = Multiplication <$> factor <*> (ws *> multiplicationToken *> ws *> term)

term :: Parser Term
term = multiplication <|> Factor <$> factor

data Expression
  = Addition Term Expression
  | Term Term
  deriving (Show, Eq)

addition :: Parser Expression
addition = Addition <$> term <*> (ws *> plusToken *> ws *> expression)

expression :: Parser Expression
expression = addition <|> Term <$> term
