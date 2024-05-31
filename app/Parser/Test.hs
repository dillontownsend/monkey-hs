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
      it "single correct parse" $ do
        let input = "let x = 5;"
            expected = Right [LetStatement $ Identifier "x"]
        parseInput input `shouldBe` expected
      it "multiple correct parse" $ do
        let input = "let x = 5; let y = 5; let z = 5;"
            expected =
              Right
                [ LetStatement $ Identifier "x",
                  LetStatement $ Identifier "y",
                  LetStatement $ Identifier "z"
                ]
        parseInput input `shouldBe` expected
      it "missing IDENT" $ do
        let input = "let = 5;"
            expected = Left $ UnexpectedToken ASSIGN
        parseInput input `shouldBe` expected
      it "missing ASSIGN" $ do
        let input = "let x 5;"
            expected = Left $ UnexpectedToken $ INT 5
        parseInput input `shouldBe` expected
      it "missing semicolon" $ do
        let input = "let x = 5"
            expected = Left MissingSemicolon
        parseInput input `shouldBe` expected

    describe "return statement" $ do
      it "single correct parse" $ do
        let input = "return 5;"
            expected = Right [ReturnStatement]
        parseInput input `shouldBe` expected
      it "multiple correct parse" $ do
        let input = "return 5; return 6; return 7;"
            expected =
              Right
                [ ReturnStatement,
                  ReturnStatement,
                  ReturnStatement
                ]
        parseInput input `shouldBe` expected
      it "missing semicolon" $ do
        let input = "return 5"
            expected = Left MissingSemicolon
        parseInput input `shouldBe` expected

    describe "identifier expression" $ do
      it "single correct parse" $ do
        let input = "x;"
            expected = Right [ExpressionStatement $ IdentifierExpression $ Identifier "x"]
        parseInput input `shouldBe` expected
      it "multiple correct parse" $ do
        let input = "x; y; z;"
            expected =
              Right
                [ ExpressionStatement $ IdentifierExpression $ Identifier "x",
                  ExpressionStatement $ IdentifierExpression $ Identifier "y",
                  ExpressionStatement $ IdentifierExpression $ Identifier "z"
                ]
        parseInput input `shouldBe` expected
      it "missing semicolon" $ do
        let input = "x"
            expected = Left MissingSemicolon
        parseInput input `shouldBe` expected

    describe "integer literal" $ do
      it "single correct parse" $ do
        let input = "5;"
            expected = Right [ExpressionStatement $ IntegerLiteral 5]
        parseInput input `shouldBe` expected
      it "multiple correct parse" $ do
        let input = "5; 6; 7;"
            expected =
              Right
                [ ExpressionStatement $ IntegerLiteral 5,
                  ExpressionStatement $ IntegerLiteral 6,
                  ExpressionStatement $ IntegerLiteral 7
                ]
        parseInput input `shouldBe` expected
      it "missing semicolon" $ do
        let input = "5"
            expected = Left MissingSemicolon
        parseInput input `shouldBe` expected
