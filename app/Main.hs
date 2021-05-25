module Main where

import           Control.Monad      ((>=>))
import           HLang              (InterpreterResult, ParserResult, interpret,
                                     parse)
import           System.Environment (getArgs)

parseFile :: FilePath -> IO (Maybe ParserResult)
parseFile fileName = parse <$> readFile fileName

interpretFile :: FilePath -> IO (Maybe InterpreterResult)
interpretFile fileName = interpret <$> readFile fileName

main :: IO ()
main = do
  args <- getArgs
  mapM_ (parseFile >=> print) args
  mapM_ (interpretFile >=> print) args
