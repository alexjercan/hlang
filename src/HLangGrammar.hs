module HLangGrammar where

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
