{
module Tokens (tokenize, Token(..)) where
}

%wrapper "basic"

$digit = 0-9                 -- digits
$alpha = [a-zA-Z]            -- alphabetic characters
$ops = [\<\>\+\-\.\#\,\:]    -- operations
$delims = [\'\(\)\[\]]       -- delimiters
$notcode = [^$ops $delims]   -- everything that isn't code

tokens :-

  $white+                            ;
  $digit+                            { \s -> TInt (read s) }
  $ops                               { \s -> TOp (head s) }
  $delims                            { \s -> TDel (head s) }
  \'[^\']*\'                         { \s -> TStr $ filter (/='\'') s}
  [$notcode $alpha $digit $white]+   ;

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
