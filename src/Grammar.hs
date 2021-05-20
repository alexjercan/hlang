module Grammar where

import           Control.Applicative (Alternative ((<|>)))
import           Lexer               (arrowToken, closedParenthesis,
                                      divisionToken, elseToken, endToken,
                                      entryToken, equalToken, fiToken,
                                      functionCallToken, ifToken, integerToken,
                                      lessThanEqualToken, lessThanToken,
                                      letToken, literalToken, minusToken,
                                      openParenthesis, plusToken, productToken,
                                      thenToken, ws, ws')
import           Parser              (Parser, charP, manyEndWith)

newtype Atom
  = Atom String
  deriving (Eq, Show)

atom :: Parser Atom
atom = Atom <$> literalToken

data Factor
  = Variable Atom
  | Integer Int
  | Parenthesis Expression
  | FunctionCall Atom Instruction
  deriving (Show, Eq)

variable :: Parser Factor
variable = Variable <$> atom

integer :: Parser Factor
integer = Integer . read <$> integerToken

parenthesis :: Parser Factor
parenthesis = openParenthesis *> ws *> (Parenthesis <$> expression) <* ws <* closedParenthesis

functionCall :: Parser Factor
functionCall = FunctionCall <$> atom <*> (ws' *> instruction) <* ws <* functionCallToken

factor :: Parser Factor
factor = functionCall <|> variable <|> integer <|> parenthesis

data Term
  = Multiplication Factor Term
  | Division Factor Term
  | Factor Factor
  deriving (Show, Eq)

multiplication :: Parser Term
multiplication = Multiplication <$> factor <*> (ws *> productToken *> ws *> term)

division :: Parser Term
division = Division <$> factor <*> (ws *> divisionToken *> ws *> term)

term :: Parser Term
term = multiplication <|> division <|> Factor <$> factor

data Expression
  = Addition Term Expression
  | Difference Term Expression
  | Term Term
  deriving (Show, Eq)

addition :: Parser Expression
addition = Addition <$> term <*> (ws *> plusToken *> ws *> expression)

difference :: Parser Expression
difference = Difference <$> term <*> (ws *> minusToken *> ws *> expression)

expression :: Parser Expression
expression = addition <|> difference <|> Term <$> term

data Condition
  = Equal Expression Expression
  | LessThan Expression Expression
  | LessThanEqual Expression Expression
  deriving (Show, Eq)

equal :: Parser Condition
equal = Equal <$> expression <*> (ws *> equalToken *> ws *> expression)

lessThan :: Parser Condition
lessThan = LessThan <$> expression <*> (ws *> lessThanToken *> ws *> expression)

lessThanEqual :: Parser Condition
lessThanEqual = LessThanEqual <$> expression <*> (ws *> lessThanEqualToken *> ws *> expression)

condition :: Parser Condition
condition = equal <|> lessThan <|> lessThanEqual

data Conditional
  = IfStatement Condition Instruction Instruction
  deriving (Show, Eq)

ifStatement :: Parser Conditional
ifStatement = ifToken *> ws' *>
              (IfStatement <$> condition <*>
                (ws' *> thenToken *> ws' *> instruction) <*>
                (ws' *> elseToken *> ws' *> instruction))
              <* ws' <* fiToken

conditional :: Parser Conditional
conditional = ifStatement

data Instruction
  = Conditional Conditional
  | Expression Expression
  | Condition Condition
  deriving (Show, Eq)

instruction :: Parser Instruction
instruction = Conditional <$> conditional <|> Expression <$> expression <|> Condition <$> condition

data Declaration
  = FunctionDeclaration Atom Atom Instruction
  deriving (Show, Eq)

functionDeclaration :: Parser Declaration
functionDeclaration = letToken *> ws' *>
                    (FunctionDeclaration <$> atom <*>
                      (ws' *> atom) <*>
                      (ws *> arrowToken *> ws *> instruction))
                    <* ws' <* endToken

declaration :: Parser Declaration
declaration = functionDeclaration

data Program
  = Program [Declaration] Instruction
  deriving (Show, Eq)

program :: Parser Program
program = Program <$>
  manyEndWith (ws <* charP ';' <* ws) declaration <*>
  (ws *> entryToken *> ws *> instruction)
