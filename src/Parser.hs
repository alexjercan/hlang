{-# LANGUAGE FlexibleInstances #-}

module Parser where

import           Control.Applicative (Alternative (..))

data Input = Input
  { readInput    :: Maybe (Char, Input)
  , indexOfInput :: Int
  } deriving (Show, Eq)

makeInput :: String -> Input
makeInput = go 0
  where
    go index [] = Input {readInput = Nothing, indexOfInput = index}
    go index (x : xs) =
      Input
        { readInput = Just (x, go (index + 1) xs)
        , indexOfInput = index
        }

data ParserError = ParserError Int String deriving (Show)

instance Alternative (Either ParserError) where
  empty = Left $ ParserError 0 "empty"
  (<|>) (Left _) r = r
  (<|>) l _        = l

newtype Parser a = Parser
  { runParser :: Input -> Either ParserError (a, Input)
  }

instance Functor Parser where
  fmap f p = Parser $ \input -> do
    (a, input') <- runParser p input
    return (f a, input')

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)
  (<*>) p1 p2 = Parser $ \input -> do
    (f, input')  <- runParser p1 input
    (a, input'') <- runParser p2 input'
    return (f a, input'')

instance Alternative Parser where
  empty = Parser $ const empty
  (<|>) p1 p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f Input {readInput = Just (y, ys), indexOfInput = index}
      | x == y = Right (x, ys)
      | otherwise = Left $ ParserError index ("Expected '" ++ [x] ++ "', but found '" ++ [y] ++ "'")
    f Input {readInput = _, indexOfInput = index} = Left $ ParserError index ("Expected '" ++ [x] ++ "', but reached end of string")

stringP :: String -> Parser String
stringP xs = Parser $ \input -> case runParser (traverse charP xs) input of
  Left _ -> Left $ ParserError (indexOfInput input) ("Expected \"" ++ xs ++ "\"")
  other  -> other

stringP' :: Parser String -> Parser String -> Parser String
stringP' r p = Parser $ \input -> case runParser r input of
  Left _ -> runParser p input
  Right (xs, _) -> Left $ ParserError (indexOfInput input) ("Reserved keyword \"" ++ xs ++ "\"")

eofP :: Parser String
eofP = Parser $ \input -> case readInput input of
  Nothing      -> Right ("", input)
  Just (x, _) -> Left $ ParserError (indexOfInput input) ("Expected end of string but found \"" ++ [x] ++ "\"")

charPredP :: String -> (Char -> Bool) -> Parser Char
charPredP desc f = Parser $ \input -> case readInput input of
  Just (x, xs)
    | f x -> Right (x, xs)
    | otherwise -> Left $ ParserError (indexOfInput input) ("Expected " ++ desc ++ " but found \"" ++ [x] ++ "\"")
  _ -> Left $ ParserError (indexOfInput input) ("Expected " ++ desc ++ " but reached end of string")

manyPredP :: String -> (Char -> Bool) -> Parser String
manyPredP desc = many . charPredP desc

somePredP :: String -> (Char -> Bool) -> Parser String
somePredP desc = some . charPredP desc

someSepBy :: Parser a -> Parser b -> Parser [b]
someSepBy sep element = (:) <$> element <*> many (sep *> element)

manySepBy :: Parser a -> Parser b -> Parser [b]
manySepBy sep element = someSepBy sep element <|> pure []

someEndWith :: Parser a -> Parser b -> Parser [b]
someEndWith end element = some (element <* end)

manyEndWith :: Parser a -> Parser b -> Parser [b]
manyEndWith end element = someEndWith end element <|> pure []

stripInput :: Either ParserError (a, Input) -> Either ParserError a
stripInput (Right (a, _)) = Right a
stripInput (Left err)     = Left err
