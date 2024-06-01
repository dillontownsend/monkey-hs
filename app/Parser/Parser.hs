module Parser.Parser (parseInput) where

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
  case (peekToken, peekToken2) of
    -- (_, SEMICOLON) -> nextToken >> return expressionStatement -- TODO: what is this for?
    (EOF, _) -> interpreterError MissingSemicolon
    (_, _) -> return expressionStatement

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
    MINUS -> PrefixExpression PrefixNegative <$> parseExpression PREFIX
    BANG -> PrefixExpression PrefixNot <$> parseExpression PREFIX
    BOOL bool -> return $ BoolLiteral bool
    LPAREN -> parseGroupedExpression
    invalidToken -> interpreterError $ InvalidNud invalidToken

parseGroupedExpression :: ParserState Expression
parseGroupedExpression = do
  expression <- parseExpression LOWEST
  peekToken <- peekNextToken
  if peekToken == RPAREN
    then nextToken >> return expression
    else interpreterError $ UnexpectedToken peekToken

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

parseLed :: Expression -> Precedence -> ParserState Expression
parseLed leftExpression leftBindingPower = do
  peekToken <- peekNextToken
  let rightBindingPower = lookupPrecedence peekToken
  peekToken2 <- peekNextToken2
  if peekToken2 /= SEMICOLON && leftBindingPower < rightBindingPower
    then case lookupInfixParseFunction peekToken of
      Nothing -> return leftExpression
      Just infixParseFunction -> do
        advancedLeftExpression <- infixParseFunction leftExpression
        parseLed advancedLeftExpression leftBindingPower
    else return leftExpression

parseInfixExpression :: Expression -> ParserState Expression
parseInfixExpression leftExpression = do
  operatorToken <- nextToken
  liftA2
    (InfixExpression leftExpression)
    (lookupInfixOperator operatorToken)
    (parseExpression $ lookupPrecedence operatorToken)

lookupInfixParseFunction :: Token -> Maybe (Expression -> ParserState Expression)
lookupInfixParseFunction PLUS = Just parseInfixExpression
lookupInfixParseFunction MINUS = Just parseInfixExpression
lookupInfixParseFunction SLASH = Just parseInfixExpression
lookupInfixParseFunction ASTERISK = Just parseInfixExpression
lookupInfixParseFunction EQUAL_TO = Just parseInfixExpression
lookupInfixParseFunction NOT_EQUAL_TO = Just parseInfixExpression
lookupInfixParseFunction LESS_THAN = Just parseInfixExpression
lookupInfixParseFunction GREATER_THAN = Just parseInfixExpression
lookupInfixParseFunction _ = Nothing

lookupInfixOperator :: Token -> ParserState InfixOperator
lookupInfixOperator PLUS = return InfixAdd
lookupInfixOperator MINUS = return InfixSubtract
lookupInfixOperator ASTERISK = return InfixMultiply
lookupInfixOperator SLASH = return InfixDivide
lookupInfixOperator GREATER_THAN = return InfixGreaterThan
lookupInfixOperator LESS_THAN = return InfixLessThan
lookupInfixOperator EQUAL_TO = return InfixEqualTo
lookupInfixOperator NOT_EQUAL_TO = return InfixNotEqualTo
lookupInfixOperator invalidToken = interpreterError $ NotAnInfixOperator invalidToken

lookupPrecedence :: Token -> Precedence
lookupPrecedence EQUAL_TO = EQUALS
lookupPrecedence NOT_EQUAL_TO = EQUALS
lookupPrecedence LESS_THAN = LESSGREATER
lookupPrecedence GREATER_THAN = LESSGREATER
lookupPrecedence PLUS = SUM
lookupPrecedence MINUS = SUM
lookupPrecedence SLASH = PRODUCT
lookupPrecedence ASTERISK = PRODUCT
lookupPrecedence _ = LOWEST

parseInput :: Input -> Either InterpreterError Program
parseInput = evalStateT parseProgram
