module Common.Types where

import Common.Trans.State
import Lexer.Token

type Input = String

type LexerState a = State Input a

type ParserState a = StateT Input (Either InterpreterError) a

data InterpreterError
  = IllegalChar Char
  | UnexpectedToken Token
  | InvalidNud Token
  | PrecedenceNotFound Token
  | NotAnInfixOperator Token

instance Show InterpreterError where
  show (IllegalChar char) = "illegal char: " ++ [char]
  show (UnexpectedToken token) = "unexpected token: " ++ show token
  show (InvalidNud token) = "no nud for token: " ++ show token
  show (PrecedenceNotFound EOF) = "expected a semicolon"
  show (PrecedenceNotFound token) = "could not find precedence for token: " ++ show token
  show (NotAnInfixOperator token) = "expected a semicolon but instead recieved: " ++ show token

interpreterError :: InterpreterError -> ParserState a
interpreterError = lift . Left
