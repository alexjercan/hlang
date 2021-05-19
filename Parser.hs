module Parser where

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f p = Parser $ \input -> do
    (input', a) <- runParser p input
    return (input', f a)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)
  (<*>) p1 p2 = Parser $ \input -> do
    (input', f) <- runParser p1 input
    (input'', a) <- runParser p2 input'
    return (input'', f a)

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | x == y = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP
