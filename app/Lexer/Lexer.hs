module Lexer.Lexer where

import Common.Trans.State
import Common.Types
import Data.Char (isAlpha, isDigit)
import Lexer.Token

advance :: ParserState ()
advance = modify tail

nextToken :: ParserState Token
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
      illegalChar -> interpreterError $ IllegalChar illegalChar

isLetter :: Char -> Bool
isLetter char = isAlpha char || char == '_'

readSingleChar :: Token -> ParserState Token
readSingleChar = (advance >>) . return

readPeekEquals :: Token -> Token -> ParserState Token
readPeekEquals tokenIfTrue tokenIfFalse = do
  advance
  input <- get
  if not $ null input || head input /= '='
    then advance >> return tokenIfTrue
    else return tokenIfFalse

getIsCurrentCharEquals :: Input -> Bool
getIsCurrentCharEquals input = not $ null input || head input /= '='

readInt :: ParserState Token
readInt = INT . read <$> seek isDigit

readIdent :: ParserState Token
readIdent = do
  ident <- seek isLetter
  case ident of
    "fn" -> return FUNCTION
    "let" -> return LET
    "if" -> return IF
    "else" -> return ELSE
    "return" -> return RETURN
    "true" -> return $ BOOL True
    "false" -> return $ BOOL False
    _ -> return $ IDENT ident

seek :: (Char -> Bool) -> ParserState Input
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

accumulateTokens :: ParserState [Token]
accumulateTokens = do
  token <- nextToken
  if token == EOF
    then return [token]
    else
      (token :) <$> accumulateTokens

lexInput :: Input -> Either InterpreterError [Token]
lexInput = evalStateT accumulateTokens
