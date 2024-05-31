module Parser.Parser where

import Common.Trans.State
import Common.Types
import Control.Applicative (liftA2)
import Control.Monad (unless)
import Lexer.Lexer (nextToken)
import Lexer.Token
import Parser.AST

parseProgram :: ParserState [Statement]
parseProgram = do
  peekToken <- peekNextToken
  if peekToken == EOF
    then return []
    else liftA2 (:) (parseStatement <* nextToken) parseProgram

parseStatement :: ParserState Statement
parseStatement = do
  token <- nextToken
  case token of
    LET -> parseLetStatement

parseLetStatement :: ParserState Statement
parseLetStatement = do
  identToken <- nextToken
  case identToken of
    IDENT ident -> do
      assignToken <- nextToken
      case assignToken of
        ASSIGN -> advanceToSemicolon >> return (LetStatement $ Identifier ident)
        unexpectedToken -> interpreterError $ UnexpectedToken unexpectedToken
    unexpectedToken -> interpreterError $ UnexpectedToken unexpectedToken

advanceToSemicolon :: ParserState ()
advanceToSemicolon = do
  peekToken <- peekNextToken
  unless (peekToken == SEMICOLON) (nextToken >> advanceToSemicolon)

peekNextToken :: ParserState Token
peekNextToken = do
  currentInput <- get
  token <- nextToken
  put currentInput
  return token

parseInput :: Input -> Either InterpreterError Program
parseInput = evalStateT parseProgram
