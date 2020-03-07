module Main where

import Grammar
import GrammarControl
import ParseArgs
import ParseInput

main = do
  (args, files) <- parseArgs
  putStrLn $ "Flags: " ++ show args
  putStrLn $ "Files: " ++ show files
  grammar <- parseInput $ head files
  validateGrammar grammar
  putStrLn "END OF PROGRAM "