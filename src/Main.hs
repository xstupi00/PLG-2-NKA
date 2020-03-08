module Main where

import Grammar
import GrammarControl
import MainControl
import ParseArgs
import ParseInput

import Control.Monad

main = do
  (args, files) <- parseArgs
--  putStrLn $ "Flags: " ++ show args
--  putStrLn $ "Files: " ++ show files
  grammar <- parseInput $ head files
  validateGrammar grammar
  when (PLG `elem` args) $ printGrammar grammar
  putStrLn "END OF PROGRAM "