module Test.EvaluatorSpec where

import Evaluator.Evaluator (evalProgram)
import Evaluator.Object
import Parser.Parser (parseInput)
import Test.Hspec

evaluatorSpec :: Spec
evaluatorSpec = do
  describe "evalProgram" $ do
    it "integer" $ do
      let input = "5;"
          expected = IntegerObject 5
          eitherProgram = parseInput input
      case eitherProgram of
        Right program -> evalProgram program `shouldBe` expected
        Left parserError -> error $ show parserError

    describe "boolean true" $ do
      it "true" $ do
        let input = "true;"
            expected = BooleanObject True
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "false" $ do
        let input = "false;"
            expected = BooleanObject False
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError

    describe "prefix expression" $ do
      it "single not bool" $ do
        let input = "!true;"
            expected = BooleanObject False
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "multiple not bool" $ do
        let input = "!!true;"
            expected = BooleanObject True
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "single negative int" $ do
        let input = "-2;"
            expected = IntegerObject (-2)
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "multiple negative int" $ do
        let input = "--2;"
            expected = IntegerObject 2
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError

    describe "infix expression" $ do
      it "int add" $ do
        let input = "1 + 2;"
            expected = IntegerObject 3
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "int subtract" $ do
        let input = "1 - 2;"
            expected = IntegerObject (-1)
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "int multiply" $ do
        let input = "2 * 2;"
            expected = IntegerObject 4
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "int divide" $ do
        let input = "10 / 2;"
            expected = IntegerObject 5
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "int greater than" $ do
        let input = "10 > 2;"
            expected = BooleanObject True
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "int less than" $ do
        let input = "10 < 2;"
            expected = BooleanObject False
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "int equal to" $ do
        let input = "10 == 2;"
            expected = BooleanObject False
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "boolean equal to" $ do
        let input = "false == false;"
            expected = BooleanObject True
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "int not equal to" $ do
        let input = "10 != 2;"
            expected = BooleanObject True
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "boolean equal to" $ do
        let input = "false != false;"
            expected = BooleanObject False
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError

    describe "if expressions" $ do
      it "true condition no alternative" $ do
        let input = "if (1 == 1) { 5; };"
            expected = IntegerObject 5
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "true condition with unused alternative" $ do
        let input = "if (1 == 1) { 5; } else { 10; };"
            expected = IntegerObject 5
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "false condition with alternative" $ do
        let input = "if (1 == 2) { 5; } else { 10; };"
            expected = IntegerObject 10
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "false condition no alternative" $ do
        let input = "if (1 == 2) { 5; };"
            expected = NullObject
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError

    describe "return statements" $ do
      it "return" $ do
        let input = "return 5;"
            expected = ReturnValue $ IntegerObject 5
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "early return" $ do
        let input = "return 5; 10; 15;"
            expected = ReturnValue $ IntegerObject 5
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
      it "nested early return" $ do
        let input = "if (true) { if (true) { return 10; }; return 1; };"
            expected = ReturnValue $ IntegerObject 10
            eitherProgram = parseInput input
        case eitherProgram of
          Right program -> evalProgram program `shouldBe` expected
          Left parserError -> error $ show parserError
