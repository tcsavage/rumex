module Main where

import Interpreter (interpret)
import Tokens (tokenize)
import Parser (parse)

import System.Environment (getArgs)
import Text.Show.Pretty (ppShow)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run ("c":args) = c args
run ("jit":args) = jit args
run ("i":args) = i args
run ("ast":args) = ast args
run _ = putStrLn "Usage:\n  Interpret: rumex i FILE [INPUT]\n  Compile: rumex c FILE\n  JIT: rumex jit FILE\n  Build AST: rumex ast FILE"

c :: [String] -> IO ()
c [file] = error "Compiler not implemented"

jit :: [String] -> IO ()
jit [file] = c [file, ""]
jit [file, inp] = error "JIT not implemented"

i :: [String] -> IO ()
i [file] = c [file, ""]
i [file, inp] = do
    src <- readFile file
    putStrLn $ interpret inp src

ast :: [String] -> IO ()
ast [file] = do
    src <- readFile file
    putStrLn $ ppShow $ parse $ tokenize src
