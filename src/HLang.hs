module HLang where

import           Grammar (Program, program)
import           Parser  (Parser (runParser), ParserError, makeInput,
                          stripInput)

newtype ParserResult = ParserResult (Either ParserError Program) deriving (Show)

parseProgram :: String -> ParserResult
parseProgram = ParserResult <$> stripInput . runParser program . makeInput
