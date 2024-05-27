module Main where

import Lexer.Lexer

main :: IO ()
main = readFile "app/main.monkey" >>= print . lexInput
