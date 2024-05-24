module Lexer.Lexer where

import Common.Trans.State
import Common.Types
import Data.Char (isAlpha, isDigit)
import Lexer.Token

advance :: LexerState ()
advance = modify tail

nextToken :: LexerState Token
nextToken = do
  modify skipWhiteSpace
  input <- get
  if null input
    then return EOF
    else case head input of
      '+' -> advance >> return PLUS
      '-' -> advance >> return MINUS
      '*' -> advance >> return ASTERISK
      '/' -> advance >> return SLASH
      '<' -> advance >> return LESS_THAN
      '>' -> advance >> return GREATER_THAN
      ',' -> advance >> return COMMA
      ';' -> advance >> return SEMICOLON
      '(' -> advance >> return LPAREN
      ')' -> advance >> return RPAREN
      '{' -> advance >> return LBRACE
      '}' -> advance >> return RBRACE
      '=' -> do
        advance
        isCurrentCharEquals <- getIsCurrentCharEquals <$> get
        if isCurrentCharEquals
          then advance >> return EQUAL_TO
          else return ASSIGN
      '!' -> do
        advance
        isCurrentCharEquals <- getIsCurrentCharEquals <$> get
        if isCurrentCharEquals
          then advance >> return NOT_EQUAL_TO
          else return BANG
      c
        | isLetter c -> readIdent
        | isDigit c -> readInt
      _ -> advance >> return ILLEGAL

isLetter :: Char -> Bool
isLetter char = isAlpha char || char == '_'

getIsCurrentCharEquals :: Input -> Bool
getIsCurrentCharEquals input = not $ null input || head input /= '='

readInt :: LexerState Token
readInt = INT . read <$> seek isDigit

readIdent :: LexerState Token
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

seek :: (Char -> Bool) -> LexerState Input
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

accumulateTokens :: LexerState [Token]
accumulateTokens = do
  token <- nextToken
  if token == EOF
    then return [token]
    else
      (token :) <$> accumulateTokens

lexInput :: Input -> [Token]
lexInput = evalState accumulateTokens
