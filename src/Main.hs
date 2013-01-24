module Main where

import Compiler (compile)
import Interpreter (interpret)
import Tokens (tokenize)
import Parser (parse)

import System.Environment (getArgs)
import Text.Show.Pretty (ppShow)

usage :: String
usage = "Usage:\n  Interpret: rumex i FILE [INPUT]\n  Compile: rumex c FILE OUTFILE\n  JIT: rumex jit FILE [INPUT]\n  Build AST: rumex ast FILE"

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run ("c":args) = c args
run ("jit":args) = jit args
run ("i":args) = i args
run ("ast":args) = ast args
run _ = putStrLn usage

c :: [String] -> IO ()
c [file, out] = do
    src <- readFile file
    compile src out
c _ = putStrLn usage

jit :: [String] -> IO ()
jit [file] = c [file, ""]
jit [file, inp] = error "JIT not implemented"
jit _ = putStrLn usage

i :: [String] -> IO ()
i [file] = i [file, ""]
i [file, inp] = do
    src <- readFile file
    putStrLn $ interpret inp src
i _ = putStrLn usage

ast :: [String] -> IO ()
ast [file] = do
    src <- readFile file
    putStrLn $ ppShow $ parse $ tokenize src
ast _ = putStrLn usage
