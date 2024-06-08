module Parser.AST where

data Statement
  = LetStatement Identifier Expression
  | ReturnStatement Expression
  | ExpressionStatement Expression
  deriving (Eq, Show)

newtype Identifier = Identifier String
  deriving (Eq)

instance Show Identifier where
  show (Identifier identifier) = identifier

data Expression
  = IdentifierExpression Identifier
  | IntegerLiteral Int
  | PrefixExpression PrefixOperator Expression
  | InfixExpression Expression InfixOperator Expression
  | BoolLiteral Bool
  | IfExpression Expression Block (Maybe Block)
  | FunctionLiteralExpression FunctionLiteral
  | CallExpression Function [Expression]
  deriving (Eq, Show)

data FunctionLiteral
  = FunctionLiteral [Identifier] Block
  deriving (Eq, Show)

data Function
  = NamedFunction Identifier
  | AnonymousFunction FunctionLiteral
  deriving (Eq, Show)

type Block = [Statement]

data PrefixOperator
  = PrefixNot
  | PrefixNegative
  deriving (Eq)

instance Show PrefixOperator where
  show PrefixNot = "!"
  show PrefixNegative = "-"

data InfixOperator
  = InfixAdd
  | InfixSubtract
  | InfixMultiply
  | InfixDivide
  | InfixGreaterThan
  | InfixLessThan
  | InfixEqualTo
  | InfixNotEqualTo
  deriving (Eq)

instance Show InfixOperator where
  show InfixAdd = "+"
  show InfixSubtract = "-"
  show InfixMultiply = "*"
  show InfixDivide = "-"
  show InfixGreaterThan = ">"
  show InfixLessThan = "<"
  show InfixEqualTo = "=="
  show InfixNotEqualTo = "!="

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
