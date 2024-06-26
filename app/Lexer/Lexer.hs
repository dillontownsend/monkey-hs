module Lexer.Lexer (lexInput, nextToken) where

import Common.Trans.State
import Common.Types
import Data.Char (isAlpha, isDigit)
import Lexer.Token

advance :: Parser ()
advance = modify tail

nextToken :: Parser Token
nextToken = do
  modify skipWhiteSpace
  input <- get
  if null input
    then return EOF
    else case head input of
      '+' -> readSingleChar PLUS
      '-' -> readSingleChar MINUS
      '*' -> readSingleChar ASTERISK
      '/' -> readSingleChar SLASH
      '<' -> readSingleChar LESS_THAN
      '>' -> readSingleChar GREATER_THAN
      ',' -> readSingleChar COMMA
      ';' -> readSingleChar SEMICOLON
      '(' -> readSingleChar LPAREN
      ')' -> readSingleChar RPAREN
      '{' -> readSingleChar LBRACE
      '}' -> readSingleChar RBRACE
      '=' -> readPeekEquals EQUAL_TO ASSIGN
      '!' -> readPeekEquals NOT_EQUAL_TO BANG
      c
        | isLetter c -> readIdent
        | isDigit c -> readInt
      illegalChar -> parserError $ IllegalChar illegalChar

isLetter :: Char -> Bool
isLetter char = isAlpha char || char == '_'

readSingleChar :: Token -> Parser Token
readSingleChar = (advance >>) . return

readPeekEquals :: Token -> Token -> Parser Token
readPeekEquals tokenIfTrue tokenIfFalse = do
  advance
  input <- get
  if not $ null input || head input /= '='
    then advance >> return tokenIfTrue
    else return tokenIfFalse

readInt :: Parser Token
readInt = INT . read <$> seek isDigit

readIdent :: Parser Token
readIdent = do
  ident <- seek isLetter
  return
    ( case ident of
        "fn" -> FUNCTION
        "let" -> LET
        "if" -> IF
        "else" -> ELSE
        "return" -> RETURN
        "true" -> BOOL True
        "false" -> BOOL False
        _ -> IDENT ident
    )

seek :: (Char -> Bool) -> Parser Input
seek predicate = do
  input <- get
  let currentChar = head input
  if null input || not (predicate currentChar)
    then return ""
    else (currentChar :) <$> (advance >> seek predicate)

skipWhiteSpace :: Input -> Input
skipWhiteSpace "" = ""
skipWhiteSpace input@(c : cs)
  | c == ' ' || c == '\t' || c == '\r' || c == '\n' = skipWhiteSpace cs
  | otherwise = input

accumulateTokens :: Parser [Token]
accumulateTokens = do
  token <- nextToken
  if token == EOF
    then return [token]
    else
      (token :) <$> accumulateTokens

lexInput :: Input -> Either ParserError [Token]
lexInput = evalStateT accumulateTokens
