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
  peekToken <- peekNextToken
  case peekToken of
    LET -> parseLetStatement
    RETURN -> parseReturnStatement
    _anyOtherToken -> parseExpressionStatement

parseExpressionStatement :: ParserState Statement
parseExpressionStatement = do
  expression <- parseExpression LOWEST
  peekToken <- peekNextToken
  let expressionStatement = return (ExpressionStatement expression)
  if peekToken == SEMICOLON
    then nextToken >> expressionStatement
    else expressionStatement

parseExpression :: Precedence -> ParserState Expression
parseExpression precedence = do
  nud <- parseNud
  let led = nud
  return led

parseNud :: ParserState Expression
parseNud = do
  token <- nextToken
  case token of
    IDENT ident -> return $ IdentifierExpression $ Identifier ident
    INT int -> return $ IntegerLiteral int
    unexpectedToken -> interpreterError $ UnexpectedToken unexpectedToken

parseLetStatement :: ParserState Statement
parseLetStatement = do
  _letToken <- nextToken
  token <- nextToken
  case token of
    IDENT ident -> do
      token' <- nextToken
      case token' of
        ASSIGN -> advanceToSemicolon >> return (LetStatement $ Identifier ident)
        unexpectedToken -> interpreterError $ UnexpectedToken unexpectedToken
    unexpectedToken -> interpreterError $ UnexpectedToken unexpectedToken

parseReturnStatement :: ParserState Statement
parseReturnStatement = nextToken >> advanceToSemicolon >> return ReturnStatement

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
