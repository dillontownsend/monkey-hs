module Parser.AST where

data Statement
  = LetStatement Identifier -- Expression
  | ReturnStatement -- Expression
  | ExpressionStatement Expression
  deriving (Eq, Show)

newtype Identifier = Identifier String
  deriving (Eq, Show)

data Expression
  = IdentifierExpression Identifier
  | IntegerLiteral Int
  deriving (Eq, Show)

data Precedence
  = LOWEST
  | EQUALS
  | LESSGREATER
  | SUM
  | PRODUCT
  | PREFIX
  | CALL
  deriving (Eq, Show, Ord)

type Program = [Statement]
