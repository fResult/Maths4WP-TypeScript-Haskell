{-# LANGUAGE LambdaCase #-}
module ParserV1 where

import Data.Char (isAlpha, isDigit, isSpace, toLower, toUpper)
import Control.Applicative (Alternative (empty, (<|>)), (*>), (<*))

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser pf) = Parser (\input ->
    case pf input of
      Just (x, rest) -> Just (f x, rest)
      Nothing        -> Nothing)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just (x, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser pf) <*> (Parser pg) = Parser $ \input ->
    case pf input of
      Nothing        -> Nothing
      Just (f, rest) -> case pg rest of
        Nothing         -> Nothing
        Just (x, rest') -> Just (f x, rest')

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser pf) <|> (Parser pg) = Parser $ \input ->
    pf input <|> pg input

-- char :: Char -> Parser Char
-- char c = Parser (\input ->
--   case input of
--     (x:xs) | x == c    -> Just(x, xs)
--     _                  -> Nothing)
char :: Char -> Parser Char
char c = Parser (\case
  (x : xs) | x == c -> Just (x, xs)
  _ -> Nothing)

insensitiveChar :: Char -> Parser Char
insensitiveChar c = fmap toLower (char (toLower c) <|> char (toUpper c))

word :: String -> Parser String
word str = token $ word' str
  where
    word' = traverse insensitiveChar

spaces :: Parser ()
spaces = Parser $ \input -> Just ((), dropWhile isSpace input)

token :: Parser a -> Parser a
token parser = spaces *> parser <* spaces
