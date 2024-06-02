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
  | PrefixExpression PrefixOperator Expression
  | InfixExpression Expression InfixOperator Expression
  | BoolLiteral Bool
  | IfExpression Expression Block (Maybe Block)
  deriving (Eq, Show)

type Block = [Statement]

data PrefixOperator
  = PrefixNot
  | PrefixNegative
  deriving (Eq, Show)

data InfixOperator
  = InfixAdd
  | InfixSubtract
  | InfixMultiply
  | InfixDivide
  | InfixGreaterThan
  | InfixLessThan
  | InfixEqualTo
  | InfixNotEqualTo
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
