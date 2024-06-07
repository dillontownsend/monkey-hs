module Evaluator.Evaluator where

import Evaluator.Object
import Parser.AST

evalProgram :: Program -> Object
evalProgram [] = NullObject
evalProgram [statement] = evalStatement statement
evalProgram (statement : statements) =
  case evalStatement statement of
    returnValue@(ReturnValue _) -> returnValue
    _ -> evalProgram statements

evalStatement :: Statement -> Object
evalStatement (ExpressionStatement expression) = evalExpression expression
evalStatement (ReturnStatement expression) = ReturnValue $ evalExpression expression

evalExpression :: Expression -> Object
evalExpression (IntegerLiteral int) = IntegerObject int
evalExpression (BoolLiteral bool) = BooleanObject bool
evalExpression (PrefixExpression prefixOperator expression) =
  evalPrefixExpression prefixOperator $ evalExpression expression
evalExpression (InfixExpression left infixOperator right) =
  evalInfixExpression (evalExpression left) infixOperator (evalExpression right)
evalExpression (IfExpression conditionExpression consequence alternative) =
  case evalExpression conditionExpression of
    BooleanObject True -> evalProgram consequence
    BooleanObject False -> maybe NullObject evalProgram alternative
    _ -> NullObject -- TODO: type typemismatch

evalPrefixExpression :: PrefixOperator -> Object -> Object
evalPrefixExpression PrefixNot (BooleanObject bool) = BooleanObject $ not bool
evalPrefixExpression PrefixNegative (IntegerObject int) = IntegerObject (-int)
evalPrefixExpression _ _ = NullObject -- TODO: implement type mismatch errors

evalInfixExpression :: Object -> InfixOperator -> Object -> Object
evalInfixExpression (IntegerObject left) InfixAdd (IntegerObject right) = IntegerObject $ left + right
evalInfixExpression (IntegerObject left) InfixSubtract (IntegerObject right) = IntegerObject $ left - right
evalInfixExpression (IntegerObject left) InfixMultiply (IntegerObject right) = IntegerObject $ left * right
evalInfixExpression (IntegerObject left) InfixDivide (IntegerObject right) = IntegerObject $ left `div` right
evalInfixExpression (IntegerObject left) InfixGreaterThan (IntegerObject right) = BooleanObject $ left > right
evalInfixExpression (IntegerObject left) InfixLessThan (IntegerObject right) = BooleanObject $ left < right
evalInfixExpression (IntegerObject left) InfixEqualTo (IntegerObject right) = BooleanObject $ left == right
evalInfixExpression (BooleanObject left) InfixEqualTo (BooleanObject right) = BooleanObject $ left == right
evalInfixExpression (IntegerObject left) InfixNotEqualTo (IntegerObject right) = BooleanObject $ left /= right
evalInfixExpression (BooleanObject left) InfixNotEqualTo (BooleanObject right) = BooleanObject $ left /= right
evalInfixExpression _ _ _ = NullObject -- TODO: implement type mismatch errors
