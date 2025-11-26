{-# LANGUAGE LambdaCase #-}
module ParserV1 where

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser (\input ->
    case p input of
      Just (a, rest) -> Just (f a, rest)
      Nothing        -> Nothing)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = undefined

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser pf) <*> (Parser pg) = undefined

-- char :: Char -> Parser Char
-- char c = Parser (\input ->
--   case input of
--     (x:xs) | x == c    -> Just(x, xs)
--     _                  -> Nothing)
char :: Char -> Parser Char
char c = Parser (\case
  (x : xs) | x == c -> Just (x, xs)
  _ -> Nothing)

word :: String -> Parser String
word = traverse char
