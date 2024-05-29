module Main where

import Lexer.Lexer

main :: IO ()
main = readFile "app/main.monkey" >>= either print print . lexInput
