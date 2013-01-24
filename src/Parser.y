{
module Parser (parse, Prog(..), Command(..), Op(..)) where

import Prelude hiding (Left, Right)
import Tokens
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  str     { TStr $$ }
  int     { TInt $$ }
  '+'     { TOp '+' }
  '-'     { TOp '-' }
  '<'     { TOp '<' }
  '>'     { TOp '>' }
  ','     { TOp ',' }
  '.'     { TOp '.' }
  '#'     { TOp '#' }
  ':'     { TOp ':' }
  '['     { TDel '[' }
  ']'     { TDel ']' }
  '('     { TDel '(' }
  ')'     { TDel ')' }

%%

Prog : Command      { CommandSequence $1 }
     | '[' Prog ']' { Loop $2 }
     | '(' Prog ')' { FunDef $2 }
     | str          { Str $1 }
     | Prog Prog    { Join $1 $2}

Command : int Op    { Command $1 $2 }
        | Op        { Command 1 $1 }

Op : '+'            { Inc }
   | '-'            { Dec }
   | '>'            { Right }
   | '<'            { Left }
   | ','            { ReadChar }
   | '.'            { WriteChar }
   | '#'            { WriteInt }
   | ':'            { Invoke }

{
parseError :: [Token] -> a
parseError ts = error $ "Parse error:\n\t" ++ show ts

data Prog = CommandSequence Command
          | Loop Prog
          | FunDef Prog
          | Str String
          | Join Prog Prog
          deriving (Show)

data Command = Command Int Op deriving (Show)

data Op = Inc
        | Dec
        | Left
        | Right
        | ReadChar
        | WriteChar
        | WriteInt
        | Invoke
        deriving (Show, Eq)
}
