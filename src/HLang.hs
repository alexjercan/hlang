module HLang where

import           HLangInterpreter (evalProgram)
import           HLangParser      (Literal, Program, parseProgram)

type ParserResult = Program

parse :: String -> Maybe ParserResult
parse str = case parseProgram str of
  Left _  -> Nothing
  Right p -> Just p

type InterpreterResult = Literal

interpret :: String -> Maybe InterpreterResult
interpret str = evalProgram =<< parse str
