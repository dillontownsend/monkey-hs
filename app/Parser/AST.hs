module Parser.AST where

data Statement
  = LetStatement Identifier -- Expression
  | ReturnStatement -- Expression
  deriving (Eq, Show)

newtype Identifier = Identifier String
  deriving (Eq, Show)

type Program = [Statement]
