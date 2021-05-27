module HLangParser where

import           Control.Applicative (Alternative ((<|>)))
import           HLangLexer          (arrowToken, atomToken, closedParenthesis,
                                      declarationEndToken, divisionToken,
                                      elseToken, endToken, entryToken, eofToken,
                                      equalToken, fiToken, functionCallToken,
                                      ifToken, integerToken, lessThanEqualToken,
                                      lessThanToken, letToken, minusToken,
                                      openParenthesis, plusToken, productToken,
                                      thenToken, ws, ws', booleanToken)
import           Parser              (Parser, ParserError, makeInput,
                                      manyEndWith, runParser, someSepBy,
                                      stripInput)

newtype Atom
  = Name String
  deriving (Show, Eq)

data Literal
  = Integer Int
  | Boolean Bool
  deriving (Show, Eq)

data Factor
  = Atom Atom
  | Literal Literal
  | Parenthesis Instruction
  deriving (Show, Eq)

data Statement
  = IfStatement Instruction Instruction Instruction
  | FunctionCall Atom Statement
  | Factor Factor
  deriving (Show, Eq)

data Term
  = Multiplication Statement Term
  | Division Statement Term
  | Statement Statement
  deriving (Show, Eq)

data Expression
  = Addition Term Expression
  | Difference Term Expression
  | Term Term
  deriving (Show, Eq)

data Instruction
  = Equal Expression Expression
  | LessThan Expression Expression
  | LessThanEqual Expression Expression
  | Expression Expression
  deriving (Show, Eq)

data Declaration
  = FunctionDeclaration Atom Atom Instruction
  deriving (Show, Eq)

data Program
  = Program [Declaration] Instruction
  deriving (Show, Eq)

name :: Parser Atom
name = Name <$> atomToken

integer :: Parser Literal
integer = Integer . read <$> integerToken

boolean :: Parser Literal
boolean = Boolean . read <$> booleanToken

parenthesis :: Parser Factor
parenthesis = openParenthesis *> ws *> (Parenthesis <$> instruction) <* ws <* closedParenthesis

ifStatement :: Parser Statement
ifStatement = ifToken *> ws' *>
              (IfStatement <$> instruction <*>
                (ws' *> thenToken *> ws' *> instruction) <*>
                (ws' *> elseToken *> ws' *> instruction))
              <* ws' <* fiToken

functionCall :: Parser Statement
functionCall = functionCallToken *> ws *> (FunctionCall <$> name <*> (ws *> statement))

multiplication :: Parser Term
multiplication = Multiplication <$> statement <*> (ws *> productToken *> ws *> term)

division :: Parser Term
division = Division <$> statement <*> (ws *> divisionToken *> ws *> term)

addition :: Parser Expression
addition = Addition <$> term <*> (ws *> plusToken *> ws *> expression)

difference :: Parser Expression
difference = Difference <$> term <*> (ws *> minusToken *> ws *> expression)

equal :: Parser Instruction
equal = Equal <$> expression <*> (ws *> equalToken *> ws *> expression)

lessThan :: Parser Instruction
lessThan = LessThan <$> expression <*> (ws *> lessThanToken *> ws *> expression)

lessThanEqual :: Parser Instruction
lessThanEqual = LessThanEqual <$> expression <*> (ws *> lessThanEqualToken *> ws *> expression)

functionDeclaration :: Parser Declaration
functionDeclaration = letToken *> ws' *>
                    (FunctionDeclaration <$> name <*>
                      (ws' *> name) <*>
                      (ws *> arrowToken *> ws *> instruction))
                    <* ws' <* endToken

factor :: Parser Factor
factor = parenthesis <|> Atom <$> name <|> Literal <$> integer <|> Literal <$> boolean

statement :: Parser Statement
statement = functionCall <|> ifStatement <|> Factor <$> factor

term :: Parser Term
term = multiplication <|> division <|> Statement <$> statement

expression :: Parser Expression
expression = addition <|> difference <|> Term <$> term

instruction :: Parser Instruction
instruction = equal <|> lessThan <|> lessThanEqual <|> Expression <$> expression

declaration :: Parser Declaration
declaration = functionDeclaration

program :: Parser Program
program = Program <$>
  manyEndWith (ws <* declarationEndToken <* ws) declaration <*>
  (ws *> entryToken *> ws *> instruction <* ws <* eofToken)

parseProgram :: String -> Either ParserError Program
parseProgram = stripInput . runParser program . makeInput
