{
module Tokens (tokenize, Token(..)) where
}

%wrapper "basic"

$digit = 0-9        -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+               ;
  $digit+               { \s -> TInt (read s) }
  [\<\>\+\-\.\#\,\:]      { \s -> TOp (head s) }
  [\(\)\[\]]            { \s -> TDel (head s) }
  \'[$alpha $digit $white \,\.\!]*\'  { \s -> TStr $ filter (/='\'') s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token = TOp Char
           | TDel Char
           | TStr String
           | TInt Int
           deriving (Eq,Show)

tokenize :: String -> [Token]
tokenize = alexScanTokens
}