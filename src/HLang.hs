module HLang where

import           HLangGrammar     (Literal, Program)
import           HLangInterpreter (evalProgram)
import           HLangParser      (parseProgram)

type ParserResult = Program

parse :: String -> Maybe ParserResult
parse str = case parseProgram str of
  Left _  -> Nothing
  Right p -> Just p

type InterpreterResult = Literal

interpret :: String -> Maybe InterpreterResult
interpret str = evalProgram =<< parse str
