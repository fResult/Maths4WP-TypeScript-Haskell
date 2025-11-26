module ParserV1 where

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
