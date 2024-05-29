module Parser.Parser where

import Common.Trans.State
import Common.Types
import Control.Applicative (liftA2)
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
  expression <- parseExpression INITIAL
  peekToken <- peekNextToken
  let expressionStatement = return (ExpressionStatement expression)
  if peekToken == SEMICOLON
    then nextToken >> expressionStatement
    else expressionStatement

parseExpression :: Precedence -> ParserState Expression
parseExpression leftBindingPower = do
  nud <- parseNud
  parseLed nud leftBindingPower

parseNud :: ParserState Expression
parseNud = do
  token <- nextToken
  case token of
    IDENT ident -> return $ IdentifierExpression $ Identifier ident
    INT int -> return $ IntegerLiteral int
    BANG -> PrefixExpression PrefixNot <$> parseExpression PREFIX
    MINUS -> PrefixExpression PrefixNegative <$> parseExpression PREFIX
    BOOL bool -> return $ BoolLiteral bool
    unexpectedToken -> interpreterError $ InvalidNud unexpectedToken

parseLed :: Expression -> Precedence -> ParserState Expression
parseLed leftExpression leftBindingPower = do
  peekToken <- peekNextToken
  rightBindingPower <- lookupPrecedence peekToken
  if leftBindingPower < rightBindingPower
    then
      nextToken >> liftA2 (InfixExpression leftExpression) (lookupInfixOperator peekToken) (parseExpression rightBindingPower)
    else return leftExpression

lookupInfixOperator :: Token -> ParserState InfixOperator
lookupInfixOperator EQUAL_TO = return InfixEqualTo
lookupInfixOperator NOT_EQUAL_TO = return InfixNotEqualTo
lookupInfixOperator LESS_THAN = return InfixLessThan
lookupInfixOperator GREATER_THAN = return InfixGreaterThan
lookupInfixOperator PLUS = return InfixAdd
lookupInfixOperator MINUS = return InfixSubtract
lookupInfixOperator SLASH = return InfixDivide
lookupInfixOperator ASTERISK = return InfixMultiply
lookupInfixOperator invalidToken = interpreterError $ NotAnInfixOperator invalidToken

lookupPrecedence :: Token -> ParserState Precedence
lookupPrecedence SEMICOLON = return LOWEST
lookupPrecedence EQUAL_TO = return EQUALS
lookupPrecedence NOT_EQUAL_TO = return EQUALS
lookupPrecedence LESS_THAN = return LESSGREATER
lookupPrecedence GREATER_THAN = return LESSGREATER
lookupPrecedence PLUS = return SUM
lookupPrecedence MINUS = return SUM
lookupPrecedence SLASH = return PRODUCT
lookupPrecedence ASTERISK = return PRODUCT
lookupPrecedence invalidToken = interpreterError $ PrecedenceNotFound invalidToken

parseLetStatement :: ParserState Statement
parseLetStatement = do
  _letToken <- nextToken
  identToken <- nextToken
  case identToken of
    IDENT ident -> do
      assignToken <- nextToken
      case assignToken of
        ASSIGN -> do
          letStatement <- LetStatement (Identifier ident) <$> parseExpression INITIAL
          _semicolonToken <- nextToken
          return letStatement
        unexpectedToken -> interpreterError $ UnexpectedToken unexpectedToken
    unexpectedToken -> interpreterError $ UnexpectedToken unexpectedToken

parseReturnStatement :: ParserState Statement
parseReturnStatement = do
  _returnToken <- nextToken
  returnStatement <- ReturnStatement <$> parseExpression INITIAL
  _semicolonToken <- nextToken
  return returnStatement

peekNextToken :: ParserState Token
peekNextToken = do
  currentInput <- get
  token <- nextToken
  put currentInput
  return token

parseInput :: Input -> Either InterpreterError Program
parseInput = evalStateT parseProgram
