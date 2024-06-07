module Common.Types where

import Common.Trans.State
import Lexer.Token
import Parser.AST (Program)

type Input = String

type LexerState a = State Input a

type ParserState a = StateT Input (Either ParserError) a

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

parserError :: ParserError -> ParserState a
parserError = lift . Left

-- type EvaluatorState a = StateT Program (Either EvaluatorError) a
--
-- data EvaluatorError
--   = TypeMismatch
--   deriving (Eq)
--
-- evaluatorError :: EvaluatorError -> EvaluatorState a
-- evaluatorError = lift . Left
