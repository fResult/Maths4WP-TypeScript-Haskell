{-# LANGUAGE LambdaCase #-}
module ParserV5 ( Parser (runParser)
                , word
                , satisfy
                , parseByLines
                , runParserUnsafe
                , char
                , parseInt
                ) where

import Data.Char (isSpace, toLower, toUpper, isDigit)
import Control.Applicative (Alternative (empty, some, (<|>)), (*>), (<*))

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- parseInt :: Parser Int
-- parseFloat :: Parser Float

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

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p) >>= f = Parser $ \input ->
    case p input of
      Nothing        -> Nothing
      Just (a, rest) -> runParser (f a) rest

char :: Char -> Parser Char
char c = satisfy (== c)

insensitiveChar :: Char -> Parser Char
insensitiveChar c = fmap toLower (char (toLower c) <|> char (toUpper c))

word :: String -> Parser String
word str = token $ traverse insensitiveChar str

spaces :: Parser ()
spaces = Parser $ \input -> Just ((), dropWhile isSpace input)

token :: Parser a -> Parser a
token parser = between spaces spaces parser

parseInt :: Parser Int
parseInt = read <$> some (satisfy isDigit)

--

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input ->
  case input of
    (c:cs) | predicate c -> Just (c, cs)
    _                    -> Nothing

between :: Parser a -> Parser b -> Parser c -> Parser c
between open close parser = open *> parser <* close

--

parseByLines :: Parser a -> Parser [a]
parseByLines parser = Parser $ \input ->
  case traverse (processLine parser) (lines input) of
    Just results -> Just (results, "")
    Nothing      -> Nothing

processLine :: Parser a -> String -> Maybe a
processLine parser str =
  case runParser parser (filter (/= '\r') str) of
    Just (results, rest)
      | all (`elem` (" \t" :: String)) rest -> Just results
      | otherwise               -> Nothing
    Nothing -> Nothing

-- For REPL testing purpose:
runParserUnsafe :: Parser a -> String -> a
runParserUnsafe p s =
  case runParser p s of
    Just (result, "") -> result
    Just _            -> error "Unconsumed input"
    Nothing           -> error "Parse error"
