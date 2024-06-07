module Evaluator.Object where

data Object
  = IntegerObject Int
  | BooleanObject Bool
  | NullObject
  | ReturnValue Object
  deriving (Eq)

instance Show Object where
  show (IntegerObject int) = show int
  show (BooleanObject True) = "true"
  show (BooleanObject False) = "false"
  show NullObject = "null"
  show (ReturnValue object) = show object
