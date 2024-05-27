module Lexer.Token where

data Token
  = EOF
  | -- identifiers + literals
    IDENT String
  | INT Int
  | BOOL Bool
  | -- operators
    ASSIGN
  | PLUS
  | MINUS
  | BANG
  | ASTERISK
  | SLASH
  | LESS_THAN
  | GREATER_THAN
  | EQUAL_TO
  | NOT_EQUAL_TO
  | -- delimeters
    COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | -- keywords
    FUNCTION
  | LET
  | IF
  | ELSE
  | RETURN
  deriving (Eq, Show)
