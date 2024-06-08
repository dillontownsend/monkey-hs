module Evaluator.Object where

import Data.List (intercalate)
import Data.Map (Map)
import Parser.AST (Block, Identifier)

type Environment = Map String Object

data Object
  = IntegerObject Int
  | BooleanObject Bool
  | NullObject
  | ReturnValue Object
  | FunctionObject [Identifier] Block Environment
  deriving (Eq)

instance Show Object where
  show (IntegerObject int) = show int
  show (BooleanObject True) = "true"
  show (BooleanObject False) = "false"
  show NullObject = "null"
  show (ReturnValue object) = show object
  show (FunctionObject identifiers _ _) = "fn(" ++ intercalate ", " (map show identifiers) ++ ")"
