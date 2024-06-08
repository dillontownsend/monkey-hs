module Main where

import Common.Trans.State (evalStateT)
import Data.Map (empty)
import Evaluator.Evaluator (evalProgram)
import Parser.Parser (parseInput)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath (takeExtension)

main :: IO () -- TODO: clean this function up
main = do
  args <- getArgs
  case args of
    [] -> error "include file path" -- TODO: this is the REPL code path
    [filePath] -> do
      fileExists <- doesFileExist filePath
      if not fileExists
        then error "file does not exist"
        else
          if takeExtension filePath /= ".monkey"
            then error $ "not a monkey file: " ++ filePath
            else do
              input <- readFile filePath
              let eitherProgram = parseInput input
              case eitherProgram of
                Left parserError -> print $ "parser error: " ++ show parserError
                Right program -> do
                  let eitherObject = evalStateT (evalProgram program) empty
                  case eitherObject of
                    Left evaluatorError -> print $ "evaluator error: " ++ show evaluatorError
                    Right object -> print object
    _ -> error "expected a single file path argument"
