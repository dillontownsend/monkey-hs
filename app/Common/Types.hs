module Common.Types where

import Common.Trans.State
import Evaluator.Object
import Lexer.Token
import Parser.AST

type Input = String

type LexerState a = State Input a

type Parser a = StateT Input (Either ParserError) a

data ParserError
  = IllegalChar Char
  | UnexpectedToken Token
  | InvalidNud Token
  | NotAnInfixOperator Token
  | MissingSemicolon
  deriving (Eq)

instance Show ParserError where
  show (IllegalChar char) = "illegal char: " ++ [char]
  show (UnexpectedToken token) = "unexpected token: " ++ show token
  show (InvalidNud token) = "no nud for token: " ++ show token
  show (NotAnInfixOperator token) = "expected a semicolon but instead recieved: " ++ show token
  show MissingSemicolon = "missing a semicolon"

parserError :: ParserError -> Parser a
parserError = lift . Left

type Evaluator a = StateT Environment (Either EvaluatorError) a

data EvaluatorError
  = PrefixExpressionTypeMismatch PrefixOperator Object
  | InfixExpressionTypeMismatch Object InfixOperator Object
  | IfExpressionTypeMismatch Object
  | UndefinedVariable String
  | IncorrectNumberOfArguments Int Int
  | NotAFunction Object
  deriving (Eq)

instance Show EvaluatorError where
  show (PrefixExpressionTypeMismatch prefixOperator object) =
    "type mismatch in prefix expression: " ++ show prefixOperator ++ show object
  show (InfixExpressionTypeMismatch left infixOperator right) =
    "type mismatch in infix expression: " ++ show left ++ " " ++ show infixOperator ++ " " ++ show right
  show (IfExpressionTypeMismatch object) =
    "type mismatch in if expression condition. expected a boolean but got: " ++ show object
  show (UndefinedVariable undefinedVariable) = "referenced undefined variable: " ++ undefinedVariable
  show (IncorrectNumberOfArguments expected actual) =
    "incorrect number of arguments. expected: " ++ show expected ++ " but recieved: " ++ show actual
  show (NotAFunction object) = "attempted to call a function on non-function value: " ++ show object

evaluatorError :: EvaluatorError -> Evaluator a
evaluatorError = lift . Left
