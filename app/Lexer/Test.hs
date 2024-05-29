module Lexer.Test where

import Common.Types
import Lexer.Lexer (lexInput)
import Lexer.Token
import Test.Hspec

testLexInput :: IO ()
testLexInput = do
  hspec $ do
    describe "lexInput" $ do
      it "single chars" $ do
        let input = "+-*/<>,;(){}"
            expected =
              Right
                [ PLUS,
                  MINUS,
                  ASTERISK,
                  SLASH,
                  LESS_THAN,
                  GREATER_THAN,
                  COMMA,
                  SEMICOLON,
                  LPAREN,
                  RPAREN,
                  LBRACE,
                  RBRACE,
                  EOF
                ]
        lexInput input `shouldBe` expected
      it "peek equals" $ do
        let input = "===!=!"
            expected =
              Right
                [ EQUAL_TO,
                  ASSIGN,
                  NOT_EQUAL_TO,
                  BANG,
                  EOF
                ]
        lexInput input `shouldBe` expected
      it "idents" $ do
        let input = "fn let if else return true false dillon"
            expected =
              Right
                [ FUNCTION,
                  LET,
                  IF,
                  ELSE,
                  RETURN,
                  BOOL True,
                  BOOL False,
                  IDENT "dillon",
                  EOF
                ]
        lexInput input `shouldBe` expected
      it "int" $ do
        let input = "15"
            expected =
              Right
                [ INT 15,
                  EOF
                ]
        lexInput input `shouldBe` expected
      it "illegal char" $ do
        let input = "$"
            expected = Left $ IllegalChar '$'
        lexInput input `shouldBe` expected
