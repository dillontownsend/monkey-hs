module Parser.Parser where

import Common.Trans.State
import Common.Types
import Control.Applicative (liftA2)
import Control.Monad (unless)
import Lexer.Lexer
import Lexer.Token
import Parser.AST

parseProgram :: ParserState Program
parseProgram = do
  peekToken <- peekNextToken
  if peekToken == EOF
    then return []
    else liftA2 (:) parseStatement parseProgram

parseStatement :: ParserState Statement
parseStatement = do
  token <- nextToken
  case token of
    LET -> parseLetStatement
    RETURN -> parseReturnStatement

parseLetStatement :: ParserState Statement
parseLetStatement = do
  token <- nextToken
  case token of
    IDENT ident -> do
      token' <- nextToken
      case token' of
        ASSIGN -> advanceToSemicolon >> return (LetStatement $ Identifier ident)
        unexpectedToken -> interpreterError $ UnexpectedToken unexpectedToken
    unexpectedToken -> interpreterError $ UnexpectedToken unexpectedToken

parseReturnStatement :: ParserState Statement
parseReturnStatement = advanceToSemicolon >> return ReturnStatement

advanceToSemicolon :: ParserState ()
advanceToSemicolon = do
  token <- nextToken
  unless (token == SEMICOLON) advanceToSemicolon

peekNextToken :: ParserState Token
peekNextToken = do
  currentInput <- get
  token <- nextToken
  put currentInput
  return token

parseInput :: Input -> Either InterpreterError Program
parseInput = evalStateT parseProgram
