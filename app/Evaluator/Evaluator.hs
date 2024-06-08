module Evaluator.Evaluator where

import Common.Trans.State
import Common.Types
import qualified Data.Map as Map (insert, lookup, union)
import Evaluator.Object
import Parser.AST

evalProgram :: Program -> Evaluator Object
evalProgram [] = return NullObject
evalProgram [statement] = evalStatement statement
evalProgram (statement : statements) = do
  object <- evalStatement statement
  case object of
    returnValue@(ReturnValue _) -> return returnValue
    _ -> evalProgram statements

evalStatement :: Statement -> Evaluator Object
evalStatement (ExpressionStatement expression) = evalExpression expression
evalStatement (ReturnStatement expression) = ReturnValue <$> evalExpression expression
evalStatement (LetStatement (Identifier identifier) expression) =
  evalExpression expression >>= setInEnvironment identifier >> return NullObject

evalExpression :: Expression -> Evaluator Object
evalExpression (IntegerLiteral int) = return $ IntegerObject int
evalExpression (BoolLiteral bool) = return $ BooleanObject bool
evalExpression (PrefixExpression prefixOperator expression) = evalExpression expression >>= evalPrefixExpression prefixOperator
evalExpression (InfixExpression left infixOperator right) = do
  leftObject <- evalExpression left
  rightObject <- evalExpression right
  evalInfixExpression leftObject infixOperator rightObject
evalExpression (IfExpression conditionExpression consequence alternative) = do
  condition <- evalExpression conditionExpression
  case condition of
    BooleanObject True -> evalProgram consequence
    BooleanObject False -> maybe (return NullObject) evalProgram alternative
    _ -> evaluatorError $ IfExpressionTypeMismatch condition
evalExpression (IdentifierExpression (Identifier identifier)) = getFromEnvironment identifier
evalExpression (FunctionLiteralExpression (FunctionLiteral identifiers block)) = FunctionObject identifiers block <$> get
evalExpression (CallExpression (NamedFunction (Identifier identifier)) expressions) = do
  object <- getFromEnvironment identifier
  case object of
    FunctionObject identifiers block environment -> do
      currentEnvironment <- get
      put $ Map.union environment currentEnvironment
      assignArgumentsToEnvironment identifiers expressions
      innerObject <- evalProgram block
      put currentEnvironment
      return innerObject
    anyOtherObject -> evaluatorError $ NotAFunction anyOtherObject
evalExpression (CallExpression (AnonymousFunction (FunctionLiteral identifiers block)) expressions) = do
  currentEnvironment <- get
  assignArgumentsToEnvironment identifiers expressions
  innerObject <- evalProgram block
  put currentEnvironment
  return innerObject

assignArgumentsToEnvironment :: [Identifier] -> [Expression] -> Evaluator ()
assignArgumentsToEnvironment parameters expressions
  | numberOfParameters /= numberOfArguments =
      evaluatorError $ IncorrectNumberOfArguments numberOfParameters numberOfArguments
  where
    numberOfParameters = length parameters
    numberOfArguments = length expressions
assignArgumentsToEnvironment [] [] = return ()
assignArgumentsToEnvironment ((Identifier key) : parameters) (argument : arguments) =
  evalExpression argument >>= setInEnvironment key >> assignArgumentsToEnvironment parameters arguments
assignArgumentsToEnvironment _ _ = error "unreachable pattern"

evalPrefixExpression :: PrefixOperator -> Object -> Evaluator Object
evalPrefixExpression PrefixNot (BooleanObject bool) = return $ BooleanObject $ not bool
evalPrefixExpression PrefixNegative (IntegerObject int) = return $ IntegerObject (-int)
evalPrefixExpression prefixOperator object = evaluatorError $ PrefixExpressionTypeMismatch prefixOperator object

evalInfixExpression :: Object -> InfixOperator -> Object -> Evaluator Object
evalInfixExpression (IntegerObject left) InfixAdd (IntegerObject right) = return $ IntegerObject $ left + right
evalInfixExpression (IntegerObject left) InfixSubtract (IntegerObject right) = return $ IntegerObject $ left - right
evalInfixExpression (IntegerObject left) InfixMultiply (IntegerObject right) = return $ IntegerObject $ left * right
evalInfixExpression (IntegerObject left) InfixDivide (IntegerObject right) = return $ IntegerObject $ left `div` right
evalInfixExpression (IntegerObject left) InfixGreaterThan (IntegerObject right) = return $ BooleanObject $ left > right
evalInfixExpression (IntegerObject left) InfixLessThan (IntegerObject right) = return $ BooleanObject $ left < right
evalInfixExpression (IntegerObject left) InfixEqualTo (IntegerObject right) = return $ BooleanObject $ left == right
evalInfixExpression (BooleanObject left) InfixEqualTo (BooleanObject right) = return $ BooleanObject $ left == right
evalInfixExpression (IntegerObject left) InfixNotEqualTo (IntegerObject right) = return $ BooleanObject $ left /= right
evalInfixExpression (BooleanObject left) InfixNotEqualTo (BooleanObject right) = return $ BooleanObject $ left /= right
evalInfixExpression left prefixOperator right = evaluatorError $ InfixExpressionTypeMismatch left prefixOperator right

getFromEnvironment :: String -> Evaluator Object
getFromEnvironment key = do
  environment <- get
  maybe (evaluatorError $ UndefinedVariable key) return (Map.lookup key environment)

setInEnvironment :: String -> Object -> Evaluator ()
setInEnvironment key object = modify $ Map.insert key object
