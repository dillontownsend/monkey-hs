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
  deriving (Eq, Show)

data PrefixOperator
  = PrefixBang
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
  | INITIAL
  | EQUALS
  | LESSGREATER
  | SUM
  | PRODUCT
  | PREFIX
  | CALL
  deriving (Eq, Show, Ord)

type Program = [Statement]
