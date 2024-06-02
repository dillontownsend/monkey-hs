module Test.ParserSpec where

import Common.Types
import Lexer.Token
import Parser.AST
import Parser.Parser (parseInput)
import Test.Hspec

parserSpec :: Spec
parserSpec = do
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
      it "missing SEMICOLON" $ do
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
      it "missing SEMICOLON" $ do
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
      it "missing SEMICOLON" $ do
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
      it "missing SEMICOLON" $ do
        let input = "5"
            expected = Left MissingSemicolon
        parseInput input `shouldBe` expected

    describe "prefix expressions" $ do
      it "single correct negative parse" $ do
        let input = "-5;"
            expected = Right [ExpressionStatement $ PrefixExpression PrefixNegative $ IntegerLiteral 5]
        parseInput input `shouldBe` expected
      it "multiple correct negative parse" $ do
        let input = "-5; -6; -7;"
            expected =
              Right
                [ ExpressionStatement $ PrefixExpression PrefixNegative $ IntegerLiteral 5,
                  ExpressionStatement $ PrefixExpression PrefixNegative $ IntegerLiteral 6,
                  ExpressionStatement $ PrefixExpression PrefixNegative $ IntegerLiteral 7
                ]
        parseInput input `shouldBe` expected
      it "single correct not parse" $ do
        let input = "!true;"
            expected = Right [ExpressionStatement $ PrefixExpression PrefixNot $ BoolLiteral True]
        parseInput input `shouldBe` expected
      it "multiple correct not parse" $ do
        let input = "!true; !false; !true;"
            expected =
              Right
                [ ExpressionStatement $ PrefixExpression PrefixNot $ BoolLiteral True,
                  ExpressionStatement $ PrefixExpression PrefixNot $ BoolLiteral False,
                  ExpressionStatement $ PrefixExpression PrefixNot $ BoolLiteral True
                ]
        parseInput input `shouldBe` expected
      it "missing expression" $ do
        let input = "!;"
            expected = Left $ InvalidNud SEMICOLON
        parseInput input `shouldBe` expected
      it "missing SEMICOLON" $ do
        let input = "!true"
            expected = Left MissingSemicolon
        parseInput input `shouldBe` expected

    describe "infix expressions" $ do
      it "single correct parse add" $ do
        let input = "5 + 5;"
            expected = Right [ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixAdd (IntegerLiteral 5)]
        parseInput input `shouldBe` expected
      it "single correct parse subtract" $ do
        let input = "5 - 5;"
            expected = Right [ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixSubtract (IntegerLiteral 5)]
        parseInput input `shouldBe` expected
      it "single correct parse multiply" $ do
        let input = "5 * 5;"
            expected = Right [ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixMultiply (IntegerLiteral 5)]
        parseInput input `shouldBe` expected
      it "single correct parse equal to" $ do
        let input = "5 == 5;"
            expected = Right [ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixEqualTo (IntegerLiteral 5)]
        parseInput input `shouldBe` expected
      it "single correct parse not equal to" $ do
        let input = "5 != 5;"
            expected = Right [ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixNotEqualTo (IntegerLiteral 5)]
        parseInput input `shouldBe` expected
      it "single correct parse less than" $ do
        let input = "5 < 5;"
            expected = Right [ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixLessThan (IntegerLiteral 5)]
        parseInput input `shouldBe` expected
      it "single correct parse greater than" $ do
        let input = "5 > 5;"
            expected = Right [ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixGreaterThan (IntegerLiteral 5)]
        parseInput input `shouldBe` expected
      it "multiple correct parse infix expressions" $ do
        let input = "5 + 5; 5 - 5; 5 * 5; 5 == 5; 5 != 5; 5 < 5; 5 > 5;"
            expected =
              Right
                [ ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixAdd (IntegerLiteral 5),
                  ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixSubtract (IntegerLiteral 5),
                  ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixMultiply (IntegerLiteral 5),
                  ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixEqualTo (IntegerLiteral 5),
                  ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixNotEqualTo (IntegerLiteral 5),
                  ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixLessThan (IntegerLiteral 5),
                  ExpressionStatement $ InfixExpression (IntegerLiteral 5) InfixGreaterThan (IntegerLiteral 5)
                ]
        parseInput input `shouldBe` expected
      it "single correct operator precedence" $ do
        let input = "a + b * c + d;"
            expected =
              Right
                [ ExpressionStatement $
                    InfixExpression
                      ( InfixExpression
                          (IdentifierExpression $ Identifier "a")
                          InfixAdd
                          ( InfixExpression
                              (IdentifierExpression $ Identifier "b")
                              InfixMultiply
                              (IdentifierExpression $ Identifier "c")
                          )
                      )
                      InfixAdd
                      (IdentifierExpression $ Identifier "d")
                ]
        parseInput input `shouldBe` expected
      it "multiple correct operator precedence" $ do
        let input = "a + b * c + d; a + b * c + d;"
            expected =
              Right
                [ ExpressionStatement $
                    InfixExpression
                      ( InfixExpression
                          (IdentifierExpression $ Identifier "a")
                          InfixAdd
                          ( InfixExpression
                              (IdentifierExpression $ Identifier "b")
                              InfixMultiply
                              (IdentifierExpression $ Identifier "c")
                          )
                      )
                      InfixAdd
                      (IdentifierExpression $ Identifier "d"),
                  ExpressionStatement $
                    InfixExpression
                      ( InfixExpression
                          (IdentifierExpression $ Identifier "a")
                          InfixAdd
                          ( InfixExpression
                              (IdentifierExpression $ Identifier "b")
                              InfixMultiply
                              (IdentifierExpression $ Identifier "c")
                          )
                      )
                      InfixAdd
                      (IdentifierExpression $ Identifier "d")
                ]
        parseInput input `shouldBe` expected
      it "missing SEMICOLON" $ do
        let input = "2 + 2"
            expected = Left MissingSemicolon
        parseInput input `shouldBe` expected

    describe "grouped expressions" $ do
      it "single correct parse" $ do
        let input = "(2);"
            expected = Right [ExpressionStatement $ IntegerLiteral 2]
        parseInput input `shouldBe` expected
      it "multiple correct parse" $ do
        let input = "(2); (a); (false);"
            expected =
              Right
                [ ExpressionStatement $ IntegerLiteral 2,
                  ExpressionStatement $ IdentifierExpression $ Identifier "a",
                  ExpressionStatement $ BoolLiteral False
                ]
        parseInput input `shouldBe` expected
      it "missing RPAREN" $ do
        let input = "(2;"
            expected = Left $ UnexpectedToken SEMICOLON
        parseInput input `shouldBe` expected
      it "missing SEMICOLON" $ do
        let input = "(2)"
            expected = Left MissingSemicolon
        parseInput input `shouldBe` expected
      it "complex grouped expression" $ do
        let input = "((1 + (2 + 3)) + 4);"
            expected =
              Right
                [ ExpressionStatement $
                    InfixExpression
                      ( InfixExpression
                          (IntegerLiteral 1)
                          InfixAdd
                          (InfixExpression (IntegerLiteral 2) InfixAdd (IntegerLiteral 3))
                      )
                      InfixAdd
                      (IntegerLiteral 4)
                ]
        parseInput input `shouldBe` expected

    describe "if expressions" $ do
      it "if expression without alternative" $ do
        let input = "if (true) { x; };"
            expected =
              Right
                [ ExpressionStatement $
                    IfExpression
                      (BoolLiteral True)
                      [ExpressionStatement $ IdentifierExpression $ Identifier "x"]
                      Nothing
                ]
        parseInput input `shouldBe` expected
      it "if expression with alternative" $ do
        let input = "if (true) { x; } else { y; };"
            expected =
              Right
                [ ExpressionStatement
                    $ IfExpression
                      (BoolLiteral True)
                      [ExpressionStatement $ IdentifierExpression $ Identifier "x"]
                    $ Just [ExpressionStatement $ IdentifierExpression $ Identifier "y"]
                ]
        parseInput input `shouldBe` expected
      it "if expression without alternative missing SEMICOLON" $ do
        let input = "if (true) { x; }"
            expected = Left MissingSemicolon
        parseInput input `shouldBe` expected
      it "if expression with alternative missing SEMICOLON" $ do
        let input = "if (true) { x; } else { y; }"
            expected = Left MissingSemicolon
        parseInput input `shouldBe` expected

    describe "complex statements" $ do
      it "test 1" $ do
        let input = "2; a + b * c + d; 1 == (2 + 3); return -(2 + 2);"
            expected =
              Right
                [ ExpressionStatement $ IntegerLiteral 2,
                  ExpressionStatement $
                    InfixExpression
                      ( InfixExpression
                          ( IdentifierExpression $
                              Identifier
                                "a"
                          )
                          InfixAdd
                          ( InfixExpression
                              (IdentifierExpression $ Identifier "b")
                              InfixMultiply
                              (IdentifierExpression $ Identifier "c")
                          )
                      )
                      InfixAdd
                      (IdentifierExpression $ Identifier "d"),
                  ExpressionStatement $
                    InfixExpression
                      (IntegerLiteral 1)
                      InfixEqualTo
                      (InfixExpression (IntegerLiteral 2) InfixAdd (IntegerLiteral 3)),
                  ReturnStatement
                ]
        parseInput input `shouldBe` expected
