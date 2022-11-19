module FunctionList where
isAlpha :: Char -> Bool
isAlpha c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\r'
