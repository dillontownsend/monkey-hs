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
  peekToken <- peekNextToken
  case peekToken of
    LET -> parseLetStatement
    RETURN -> parseReturnStatement
    _ -> parseExpressionStatement

parseLetStatement :: ParserState Statement
parseLetStatement = do
  _letToken <- nextToken
  identToken <- nextToken
  case identToken of
    IDENT ident -> do
      assignToken <- nextToken
      case assignToken of
        ASSIGN -> advanceToSemicolon >> return (LetStatement $ Identifier ident)
        unexpectedToken -> interpreterError $ UnexpectedToken unexpectedToken
    unexpectedToken -> interpreterError $ UnexpectedToken unexpectedToken

parseReturnStatement :: ParserState Statement
parseReturnStatement = nextToken >> advanceToSemicolon >> return ReturnStatement

parseExpressionStatement :: ParserState Statement
parseExpressionStatement = do
  expressionStatement <- ExpressionStatement <$> parseExpression LOWEST
  peekToken <- peekNextToken
  peekToken2 <- peekNextToken2
  if peekToken /= SEMICOLON && peekToken2 /= SEMICOLON
    then interpreterError MissingSemicolon
    else
      if peekToken2 == SEMICOLON -- TODO: clean this up
        then nextToken >> return expressionStatement
        else return expressionStatement

parseExpression :: Precedence -> ParserState Expression
parseExpression rightBindingPower = do
  nud <- parseNud
  let left = nud
  return nud

parseNud :: ParserState Expression
parseNud = do
  token <- nextToken
  case token of
    IDENT ident -> return $ IdentifierExpression $ Identifier ident
    INT int -> return $ IntegerLiteral int

advanceToSemicolon :: ParserState ()
advanceToSemicolon = do
  peekToken <- peekNextToken
  if peekToken == EOF
    then interpreterError MissingSemicolon
    else unless (peekToken == SEMICOLON) (nextToken >> advanceToSemicolon)

peekNextToken :: ParserState Token
peekNextToken = do
  currentInput <- get
  token <- nextToken
  put currentInput
  return token

peekNextToken2 :: ParserState Token
peekNextToken2 = do
  currentInput <- get
  _token1 <- nextToken
  token2 <- nextToken
  put currentInput
  return token2

parseInput :: Input -> Either InterpreterError Program
parseInput = evalStateT parseProgram
