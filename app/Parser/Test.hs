module Parser.Test where

import Common.Types
import Lexer.Token
import Parser.AST
import Parser.Parser (parseInput)
import Test.Hspec

testParseInput :: IO ()
testParseInput = hspec $ do
  describe "parseInput" $ do
    describe "let statement" $ do
      it "correct parse" $ do
        let input = "let x = 5;"
            expected = Right [LetStatement $ Identifier "x"]
        parseInput input `shouldBe` expected
      it "missing IDENT" $ do
        let input = "let = 5;"
            expected = Left $ UnexpectedToken ASSIGN
        parseInput input `shouldBe` expected
      it "missing ASSIGN" $ do
        let input = "let x 5;"
            expected = Left $ UnexpectedToken $ INT 5
        parseInput input `shouldBe` expected

    describe "return statement" $ do
      it "correct parse" $ do
        let input = "return 5;"
            expected = Right [ReturnStatement]
        parseInput input `shouldBe` expected
