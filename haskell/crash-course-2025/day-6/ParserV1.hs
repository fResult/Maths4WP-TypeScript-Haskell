{-# LANGUAGE LambdaCase #-}
module ParserV1 where

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- char :: Char -> Parser Char
-- char c = Parser (\input ->
--   case input of
--     (x:xs) | x == c    -> Just(x, xs)
--     _                  -> Nothing)
char :: Char -> Parser Char
char c = Parser (\case
  (x : xs) | x == c -> Just (x, xs)
  _ -> Nothing)
