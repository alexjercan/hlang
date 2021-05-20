module Main where

import           Control.Monad      ((>=>))
import           HLang              (ParserResult, parseProgram)
import           System.Environment (getArgs)

parseFile :: FilePath -> IO ParserResult
parseFile fileName = parseProgram <$> readFile fileName

main :: IO ()
main = do
  args <- getArgs
  mapM_ (parseFile >=> print) args
