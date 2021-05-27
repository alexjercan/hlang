module HLangParser where

import           Control.Applicative (Alternative ((<|>)))
import           HLangGrammar        (Atom (..), Declaration (..),
                                      Expression (..), Factor (..),
                                      Instruction (..), Literal (..),
                                      Program (..), Statement (..), Term (..))
import           HLangLexer          (arrowToken, atomToken, booleanToken,
                                      closedParenthesis, declarationEndToken,
                                      divisionToken, elseToken, endToken,
                                      entryToken, eofToken, equalToken, fiToken,
                                      functionCallToken, ifToken, integerToken,
                                      lessThanEqualToken, lessThanToken,
                                      letToken, minusToken, openParenthesis,
                                      plusToken, productToken, thenToken, ws,
                                      ws')
import           Parser              (Parser, ParserError, makeInput,
                                      manyEndWith, runParser, someSepBy,
                                      stripInput)

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
