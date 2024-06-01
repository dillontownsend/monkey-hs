module Test.MainSpec where

import Test.Hspec
import Test.LexerSpec (lexerSpec)
import Test.ParserSpec (parserSpec)

main :: IO ()
main = hspec $ lexerSpec >> parserSpec
