module Common.Types where

import Common.Trans.State
import Lexer.Token

type Input = String

type LexerState a = State Input a

type ParserState a = StateT Input (Either InterpreterError) a

data InterpreterError
  = IllegalChar Char
  | UnexpectedToken Token

instance Show InterpreterError where
  show (IllegalChar char) = "illegal char: " ++ [char]
  show (UnexpectedToken token) = "unexpected token: " ++ show token

interpreterError :: InterpreterError -> ParserState a
interpreterError = lift . Left
