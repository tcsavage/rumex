{
module Tokens (tokenize, Token(..)) where
}

%wrapper "basic"

$digit = 0-9                        -- digits
$alpha = [a-zA-Z]                   -- alphabetic characters
$ops = [\<\>\+\-\.\#\,\:]           -- operations
$strdelim = [\"]                    -- string delimiter
$delims = [$strdelim \( \) \[ \]]   -- delimiters
$code = [$ops $delims $digit]       -- everything that can be called code

tokens :-

  $white+                            ;
  $digit+                            { \s -> TInt (read s) }
  $ops                               { \s -> TOp (head s) }
  $delims                            { \s -> TDel (head s) }
  $strdelim[^$strdelim]*$strdelim    { \s -> TStr $ filter (/='\'') s}
  "{*" .* "*}"                       ;

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
