module Main where

import Interpreter

import System.Environment (getArgs)
import System.IO (readFile)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run ["c", file] = error "Compiler not implemented"
run ["i", file, inp] = do
    src <- readFile file
    putStrLn $ interpret src inp
run ["i", file] = do
    src <- readFile file
    putStrLn $ interpret src ""
run _ = putStrLn "Usage:\n  Interpret: rumex i FILE [INPUT]\n  Compile: rumex c FILE"
