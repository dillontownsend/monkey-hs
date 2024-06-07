module Main where

import Common.Trans.State (evalStateT)
import Data.Map (empty)
import Evaluator.Evaluator (evalProgram)
import Parser.Parser (parseInput)

main :: IO ()
main = do
  input <- readFile "app/main.monkey"
  let eitherProgram = parseInput input
  case eitherProgram of
    Left parserError -> print parserError
    Right program -> do
      let eitherObject = evalStateT (evalProgram program) empty
      case eitherObject of
        Left evaluatorError -> print evaluatorError
        Right object -> print object
