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

parseLed :: Expression -> Precedence -> ParserState Expression
parseLed leftExpression leftBindingPower = do
  token <- nextToken
  rightBindingPower <- lookupPrecedence token
  if leftBindingPower < rightBindingPower
    then
      liftA2 (InfixExpression leftExpression) (lookupInfixOperator token) (parseExpression rightBindingPower)
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

parseNud :: ParserState Expression
parseNud = do
  token <- nextToken
  case token of
    IDENT ident -> return $ IdentifierExpression $ Identifier ident
    INT int -> return $ IntegerLiteral int
    BANG -> PrefixExpression PrefixBang <$> parseExpression PREFIX
    MINUS -> PrefixExpression PrefixNegative <$> parseExpression PREFIX
    unexpectedToken -> interpreterError $ InvalidNud unexpectedToken

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
