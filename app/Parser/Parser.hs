module Parser.Parser (parseInput) where

import Common.Trans.State
import Common.Types
import Control.Applicative (liftA2)
import Lexer.Lexer (nextToken)
import Lexer.Token
import Parser.AST

parseProgram :: ParserState Program
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
parseLetStatement =
  liftA2
    (LetStatement . Identifier)
    (nextToken >> expectIdent <* expectNextToken ASSIGN)
    (parseExpression LOWEST)
    <* expectPeekTokenSemicolon

parseReturnStatement :: ParserState Statement
parseReturnStatement = ReturnStatement <$> (nextToken >> parseExpression LOWEST) <* expectPeekTokenSemicolon

parseExpressionStatement :: ParserState Statement
parseExpressionStatement = ExpressionStatement <$> parseExpression LOWEST <* expectPeekTokenSemicolon

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
    IF -> parseIfExpression
    FUNCTION -> parseFunctionLiteral
    invalidToken -> parserError $ InvalidNud invalidToken

parseCallExpression :: Expression -> ParserState Expression
parseCallExpression (FunctionLiteralExpression function) =
  CallExpression (AnonymousFunction function) <$> (nextToken >> parseCallArguments)
parseCallExpression (IdentifierExpression identifier) =
  CallExpression (NamedFunction identifier) <$> (nextToken >> parseCallArguments)
parseCallExpression _ = parserError $ UnexpectedToken LPAREN

parseCallArguments :: ParserState [Expression]
parseCallArguments = do
  peekToken <- peekNextToken
  if peekToken == RPAREN
    then nextToken >> return []
    else do
      let expressions = liftA2 (:) (parseExpression LOWEST) parseCallArguments
      peekToken' <- peekNextToken
      if peekToken' == COMMA
        then nextToken >> expressions
        else expressions

parseFunctionLiteral :: ParserState Expression
parseFunctionLiteral = do
  expectNextToken LPAREN
  parameters <- parseFunctionParameters
  expectNextToken LBRACE
  body <- parseBlock
  expectNextToken RBRACE
  return $ FunctionLiteralExpression $ FunctionLiteral parameters body

parseFunctionParameters :: ParserState [Identifier]
parseFunctionParameters = do
  token <- nextToken
  case token of
    RPAREN -> return []
    IDENT ident -> do
      peekToken <- peekNextToken
      let identifiers = (Identifier ident :) <$> parseFunctionParameters
      if peekToken == COMMA
        then nextToken >> identifiers
        else identifiers
    unexpectedToken -> parserError $ UnexpectedToken unexpectedToken

parseIfExpression :: ParserState Expression
parseIfExpression = do
  expectNextToken LPAREN
  condition <- parseExpression LOWEST
  expectNextToken RPAREN
  expectNextToken LBRACE
  consequence <- parseBlock
  expectNextToken RBRACE
  peekToken <- peekNextToken
  let ifExpressionBeforeConsequence = IfExpression condition consequence
  if peekToken == ELSE
    then do
      _elseToken <- nextToken
      expectNextToken LBRACE
      alternative <- parseBlock
      expectNextToken RBRACE
      return $ ifExpressionBeforeConsequence $ Just alternative
    else return $ ifExpressionBeforeConsequence Nothing

parseBlock :: ParserState Block
parseBlock = do
  peekToken <- peekNextToken
  if peekToken /= RBRACE && peekToken /= EOF
    then liftA2 (:) (parseStatement <* nextToken) parseBlock
    else return []

parseGroupedExpression :: ParserState Expression
parseGroupedExpression = parseExpression LOWEST <* expectNextToken RPAREN

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
lookupInfixParseFunction LPAREN = Just parseCallExpression
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
lookupInfixOperator invalidToken = parserError $ NotAnInfixOperator invalidToken

lookupPrecedence :: Token -> Precedence
lookupPrecedence EQUAL_TO = EQUALS
lookupPrecedence NOT_EQUAL_TO = EQUALS
lookupPrecedence LESS_THAN = LESSGREATER
lookupPrecedence GREATER_THAN = LESSGREATER
lookupPrecedence PLUS = SUM
lookupPrecedence MINUS = SUM
lookupPrecedence SLASH = PRODUCT
lookupPrecedence ASTERISK = PRODUCT
lookupPrecedence LPAREN = CALL
lookupPrecedence _ = LOWEST

expectNextToken :: Token -> ParserState ()
expectNextToken expectedToken = do
  token <- nextToken
  if token == expectedToken
    then return ()
    else parserError $ UnexpectedToken token

expectPeekTokenSemicolon :: ParserState ()
expectPeekTokenSemicolon = do
  peekToken <- peekNextToken
  if peekToken == SEMICOLON
    then return ()
    else parserError MissingSemicolon

expectIdent :: ParserState String
expectIdent = do
  token <- nextToken
  case token of
    IDENT ident -> return ident
    unexpectedToken -> parserError $ UnexpectedToken unexpectedToken

parseInput :: Input -> Either ParserError Program
parseInput = evalStateT parseProgram
